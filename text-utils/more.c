
/*
 * Copyright (C) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

/* more.c - General purpose tty output filter and file perusal program
 *
 * by Eric Shienbrood, UC Berkeley
 *
 * modified by Geoff Peck
 *	UCB to add underlining, single spacing
 * modified by John Foderaro
 *	UCB to add -c and MORE environment variable
 * modified by Erik Troan <ewt@redhat.com>
 *	to be more posix and so compile on linux/axp.
 * modified by Kars de Jong <jongk@cs.utwente.nl>
 *	to use terminfo instead of termcap.
 * 1999-02-22 Arkadiusz Miśkiewicz <misiek@pld.ORG.PL>
 *	added Native Language Support
 * 1999-03-19 Arnaldo Carvalho de Melo <acme@conectiva.com.br>
 *	more nls translatable strings
 * 1999-05-09 aeb
 *	applied a RedHat patch (setjmp->sigsetjmp); without it a second
 *	^Z would fail.
 * 1999-05-09 aeb
 *	undone Kars' work, so that more works without libcurses (and
 *	hence can be in /bin with libcurses being in
 *	/usr/lib which may not be mounted).  However, when termcap is not
 *	present curses can still be used.
 * 2010-10-21 Davidlohr Bueso <dave@gnu.org>
 *	modified mem allocation handling for util-linux
 */

/* system includes */
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <paths.h>
#include <poll.h>
#include <regex.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/param.h>
#include <sys/signalfd.h>
#include <sys/stat.h>
#include <sys/ttydefaults.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#ifdef HAVE_MAGIC
# include <magic.h>
#endif

#include <term.h>		/* include after <.*curses.h> */

/* util-linux includes */
#include "c.h"
#include "closestream.h"
#include "nls.h"
#include "rpmatch.h"
#include "strutils.h"
#include "widechar.h"
#include "xalloc.h"

/* definitions */
#ifdef TEST_PROGRAM
# define NON_INTERACTIVE_MORE 1
#endif

#define VI	"vi"	/* found on the user's path */

#define LINSIZ		256	/* minimal Line buffer size */
#define ESC		'\033'
#define SCROLL_LEN	11
#define LINES_PER_PAGE	24
#define NUM_COLUMNS	80
#define INIT_BUF	80
#define SHELL_LINE	1000
#define COMMAND_BUF	200
#define REGERR_BUF	NUM_COLUMNS
#define SEARCH_TIMEOUT	10

#define TERM_AUTO_RIGHT_MARGIN    "am"
#define TERM_CEOL                 "xhp"
#define TERM_CLEAR                "clear"
#define TERM_CLEAR_TO_LINE_END    "el"
#define TERM_CLEAR_TO_SCREEN_END  "ed"
#define TERM_COLS                 "cols"
#define TERM_CURSOR_ADDRESS       "cup"
#define TERM_EAT_NEW_LINE         "xenl"
#define TERM_ENTER_BOLD           "bold"
#define TERM_ENTER_UNDERLINE      "smul"
#define TERM_EXIT_BOLD            "sgr0"
#define TERM_EXIT_STANDARD_MODE   "rmso"
#define TERM_EXIT_UNDERLINE       "rmul"
#define TERM_HARD_COPY            "hc"
#define TERM_HOME                 "home"
#define TERM_LINE_DOWN            "cud1"
#define TERM_LINES                "lines"
#define TERM_OVER_STRIKE          "os"
#define TERM_STANDARD_MODE        "smso"
#define TERM_STD_MODE_GLITCH      "xmc"
#define TERM_UNDERLINE_CHAR       "uc"
#define TERM_UNDERLINE            "ul"

/* control struct */
struct more_control {
	struct termios output_tty;	/* output terminal */
	struct termios orig_tty;	/* original terminal settings */
	off_t file_pos;			/* file position */
	off_t file_size;		/* file size */
	int argv_position;		/* position in argv[] */
	int d_scroll_len;		/* number of lines scrolled by 'd' */
	int lines_per_screen;		/* screen size in lines */
	int promptlen;			/* message prompt length */
	int jump_len;			/* line number to jump */
	int current_line;		/* line we are currently at */
	char **file_names;		/* the list of file names */
	int num_files;			/* number of files left to process */
	char *shell;			/* the name of the shell to use */
	int shellp;			/* does previous shell command exists */
	int sigfd;			/* signalfd() file descriptor */
	char *linebuf;			/* line buffer */
	size_t linesz;			/* size of line buffer */
	int lines_per_page;		/* lines per page */
	char *clear;			/* clear screen */
	char *eraseln;			/* erase line */
	char *std_enter;		/* enter standout mode */
	char *std_exit;			/* exit standout mode */
	char *underline_enter;		/* enter underline mode */
	char *underline_exit;		/* exit underline mode */
	char *bold_enter;		/* enter bold mode */
	char *bold_exit;		/* exit bold mode */
	char *go_home;			/* go to screen home position */
	char *end_clear;		/* clear rest of screen */
	int num_columns;		/* number of columns */
	char *previousre;		/* previous search() buf[] item */
	struct {
		long row_num;		/* row number */
		long line_num;		/* line number */
	} context,
	  screen_start;
	int lastcmd;			/* previous more key command */
	int lastarg;			/* previous key command argument */
	int lastcolon;			/* is a colon-prefixed key command */
	char shell_line[SHELL_LINE];
#ifdef HAVE_MAGIC
	magic_t magic;			/* libmagic data load */
#endif
	unsigned int
		catch_susp:1,		/* should SIGTSTP signal be caught */
		clearfirst:1,		/* is first character in file \f */
		clreol_opt:1,		/* do not scroll, paint each screen from the top */
		docrterase:1,		/* is erase previous supported */
		dumb:1,			/* is terminal type known */
		graphics_mode:1,	/* is input in middle of Ecma-048 SGR */
		eatnl:1,		/* is newline ignored after 80 cols */
		errors:1,		/* is an error reported */
		first_file:1,		/* is the input file the first in list */
		fold_opt:1,		/* fold long lines */
		hard_term:1,		/* is this hard copy terminal */
		hardtabs:1,		/* print spaces instead of '\t' */
		ignore_case_opt:1,	/* case insensitive pattern search */
		is_paused:1,		/* is output paused */
		jump_defined:1,		/* is jumpline defined */
		no_bell:1,		/* suppress bell */
		no_intty:1,		/* is input in interactive mode */
		no_tty:1,		/* is output in interactive mode */
		noscroll_opt:1,		/* do not scroll, clear the screen and then display text */
		notell:1,		/* suppress quit dialog */
		print_names:1,		/* print file name banner */
		rerun_command:1,	/* run previous key command */
		search_called:1,	/* previous more command was a search */
		search_opt:1,		/* is init search pattern defined */
		squeeze_opt:1,		/* suppress white space */
		stdout_glitch:1,	/* terminal has standout mode glitch */
		stop_opt:1,		/* stop after form feeds */
		ul_opt:1,		/* underline as best we can */
		wrap_margin:1;		/* set if automargins */
};

/* functions till end of file */
static void __attribute__((__noreturn__)) usage(FILE *out)
{
	fputs(USAGE_HEADER, out);
	fprintf(out, _(" %s [options] <file>...\n"), program_invocation_short_name);

	fputs(USAGE_SEPARATOR, out);
	fputs(_("A file perusal filter for CRT viewing.\n"), out);

	fputs(USAGE_OPTIONS, out);
	fputs(_(" -d          display help instead of ringing bell\n"), out);
	fputs(_(" -f          count logical rather than screen lines\n"), out);
	fputs(_(" -l          suppress pause after form feed\n"), out);
	fputs(_(" -c          do not scroll, display text and clean line ends\n"), out);
	fputs(_(" -p          do not scroll, clean screen and display text\n"), out);
	fputs(_(" -s          squeeze multiple blank lines into one\n"), out);
	fputs(_(" -u          suppress underlining and bold\n"), out);
	fputs(_(" -i          ignore case when searching\n"), out);
	fputs(_(" -<number>   the number of lines per screenful\n"), out);
	fputs(_(" -n <number> same as -<number>\n"), out);
	fputs(_(" +<number>   display file beginning from line number\n"), out);
	fputs(_(" +/<string>  display file beginning from search string match\n"), out);
	fputs(_(" -V          display version information and exit\n"), out);
	fprintf(out, USAGE_MAN_TAIL("more(1)"));
	exit(out == stderr ? EXIT_FAILURE : EXIT_SUCCESS);
}

static void argscan(struct more_control *ctl, char *s)
{
	int seen_num = 0;

	while (*s != '\0') {
		switch (*s) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if (!seen_num) {
				ctl->lines_per_screen = 0;
				seen_num = 1;
			}
			ctl->lines_per_screen = ctl->lines_per_screen * 10 + *s - '0';
			break;
		case 'n':
			ctl->num_files--;
			if (!*++ctl->file_names)
				errx(EXIT_FAILURE, "%s -- '%c'", _("option requires an argument"), 'n');
			ctl->lines_per_screen = strtou16_or_err(*ctl->file_names, _("argument error"));
			break;
		case 'd':
			ctl->no_bell = 1;
			break;
		case 'l':
			ctl->stop_opt = 0;
			break;
		case 'f':
			ctl->fold_opt = 0;
			break;
		case 'p':
			ctl->noscroll_opt = 1;
			break;
		case 'c':
			ctl->clreol_opt = 1;
			break;
		case 's':
			ctl->squeeze_opt = 1;
			break;
		case 'u':
			ctl->ul_opt = 0;
			break;
		case 'i':
			ctl->ignore_case_opt = 1;
			break;
		case 'e':
			break;
		case '-':
		case ' ':
		case '\t':
			break;
		case 'V':
			printf(UTIL_LINUX_VERSION);
			exit(EXIT_SUCCESS);
			break;
		default:
			warnx(_("unknown option -%s"), s);
			usage(stderr);
			break;
		}
		s++;
	}
}

#ifdef HAVE_MAGIC
/* check_magic -- check for file magic numbers. */
static int check_magic(struct more_control *ctl, FILE *f, char *fs)
{
	const char *mime_encoding = magic_descriptor(ctl->magic, fileno(f));

	if (!mime_encoding || !(strcmp("binary", mime_encoding))) {
		printf(_("\n******** %s: Not a text file ********\n\n"), fs);
		return 1;
	}
	return 0;
}
#endif

static void more_fseek(struct more_control *ctl, FILE *stream, off_t pos)
{
	ctl->file_pos = pos;
	fseeko(stream, pos, SEEK_SET);
}

static int more_getc(struct more_control *ctl, FILE *stream)
{
	ctl->file_pos++;
	return getc(stream);
}

static int more_ungetc(struct more_control *ctl, int c, FILE *stream)
{
	ctl->file_pos--;
	return ungetc(c, stream);
}

/* Check whether the file named by fs is an ASCII file which the user may
 * access.  If it is, return the opened file.  Otherwise return NULL. */
static FILE *more_fopen(struct more_control *ctl, char *fs)
{
	struct stat stbuf;
	FILE *f;
	int c;

	fflush(NULL);
	if (stat(fs, &stbuf) == -1) {
		warn(_("stat of %s failed"), fs);
		return NULL;
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		printf(_("\n*** %s: directory ***\n\n"), fs);
		return NULL;
	}
	if ((stbuf.st_mode & S_IFMT) != S_IFREG) {
		printf(_("\n*** %s: is not a file ***\n\n"), fs);
		return NULL;
	}
	if ((f = fopen(fs, "r")) == NULL) {
		warn(_("cannot open %s"), fs);
		return NULL;
	}
#ifdef HAVE_MAGIC
	/* libmacig classifies empty file as binary, that is not what is
	 * wanted here */
	if (stbuf.st_size && check_magic(ctl, f, fs)) {
		fclose(f);
		return NULL;
	}
#endif
	fcntl(fileno(f), F_SETFD, FD_CLOEXEC);
	c = more_getc(ctl, f);
	ctl->clearfirst = (c == '\f');
	more_ungetc(ctl, c, f);
	if ((ctl->file_size = stbuf.st_size) == 0)
		ctl->file_size = LONG_MAX;
	return f;
}

static void prepare_line_buffer(struct more_control *ctl)
{
	size_t nsz = ctl->num_columns * 4;

	if (nsz <= ctl->linesz)
		return;

	if (nsz < LINSIZ)
		nsz = LINSIZ;

	/* alloc nsz and extra space for \n\0 */
	ctl->linebuf = xrealloc(ctl->linebuf, nsz + 2);
	ctl->linesz = nsz;
}

/* Get a logical line */
static int get_line(struct more_control *ctl, FILE *f, int *length)
{
	int c;
	char *p = ctl->linebuf;
	int column = 0;
	static int colflg;

#ifdef HAVE_WIDECHAR
	size_t i;
	wchar_t wc;
	int wc_width;
	mbstate_t state, state_bak;	/* Current status of the stream. */
	char mbc[MB_LEN_MAX];		/* Buffer for one multibyte char. */
	size_t mblength;		/* Byte length of multibyte char. */
	size_t mbc_pos = 0;		/* Position of the MBC. */
	int use_mbc_buffer_flag = 0;	/* If 1, mbc has data. */
	int break_flag = 0;		/* If 1, exit while(). */
	off_t file_pos_bak = ctl->file_pos;

	memset(&state, 0, sizeof state);
#endif
	c = more_getc(ctl, f);
	if (colflg && c == '\n') {
		ctl->current_line++;
		c = more_getc(ctl, f);
	}
	while (p < &ctl->linebuf[ctl->linesz - 1]) {
#ifdef HAVE_WIDECHAR
		if (ctl->fold_opt && use_mbc_buffer_flag && 1 < MB_CUR_MAX) {
			use_mbc_buffer_flag = 0;
			state_bak = state;
			mbc[mbc_pos++] = c;
 process_mbc:
			mblength = mbrtowc(&wc, mbc, mbc_pos, &state);

			switch (mblength) {
			case (size_t)-2:	/* Incomplete multibyte character. */
				use_mbc_buffer_flag = 1;
				state = state_bak;
				break;

			case (size_t)-1:	/* Invalid as a multibyte character. */
				*p++ = mbc[0];
				state = state_bak;
				column++;
				file_pos_bak++;

				if (ctl->num_columns <= column)
					more_fseek(ctl, f, file_pos_bak);
				else {
					memmove(mbc, mbc + 1, --mbc_pos);
					if (0 < mbc_pos) {
						mbc[mbc_pos] = '\0';
						goto process_mbc;
					}
				}
				break;

			default:
				wc_width = wcwidth(wc);
				if (ctl->num_columns < (column + wc_width)) {
					more_fseek(ctl, f, file_pos_bak);
					break_flag = 1;
				} else {
					for (i = 0; p < &ctl->linebuf[ctl->linesz - 1] &&
						    i < mbc_pos; i++)
						*p++ = mbc[i];
					if (0 < wc_width)
						column += wc_width;
				}
			}

			if (break_flag || ctl->num_columns <= column)
				break;

			c = more_getc(ctl, f);
			continue;
		}
#endif	/* HAVE_WIDECHAR */
		if (c == EOF) {
			if (ctl->linebuf < p) {
				*p = '\0';
				*length = p - ctl->linebuf;
				return column;
			}
			*length = p - ctl->linebuf;
			return EOF;
		}
		if (c == '\n') {
			ctl->current_line++;
			break;
		}

		*p++ = c;
		if (c == '\t') {
			if (!ctl->hardtabs || (column < ctl->promptlen && !ctl->hard_term)) {
				if (ctl->hardtabs && ctl->eraseln && !ctl->dumb) {
					column = 1 + (column | 7);
					putp(ctl->eraseln);
					ctl->promptlen = 0;
				} else {
					for (--p; p < &ctl->linebuf[ctl->linesz - 1];) {
						*p++ = ' ';
						if ((++column & 7) == 0)
							break;
					}
					if (ctl->promptlen <= column)
						ctl->promptlen = 0;
				}
			} else
				column = 1 + (column | 7);
		} else if (c == '\b' && 0 < column) {
			column--;
		} else if (c == '\r') {
			int next = more_getc(ctl, f);
			if (next == '\n') {
				p--;
				ctl->current_line++;
				break;
			}
			more_ungetc(ctl, next, f);
			column = 0;
		} else if (c == '\f' && ctl->stop_opt) {
			p[-1] = '^';
			*p++ = 'L';
			column += 2;
			ctl->is_paused = 1;
		} else if (c == EOF) {
			*length = p - ctl->linebuf;
			return column;
		} else {
#ifdef HAVE_WIDECHAR
			if (ctl->fold_opt && 1 < MB_CUR_MAX) {
				memset(mbc, 0, MB_LEN_MAX);
				mbc_pos = 0;
				mbc[mbc_pos++] = c;
				state_bak = state;

				mblength = mbrtowc(&wc, mbc, mbc_pos, &state);
				/* The value of mblength is always less than 2 here. */
				switch (mblength) {
				case (size_t)-2:
					p--;
					file_pos_bak = ctl->file_pos - 1;
					state = state_bak;
					use_mbc_buffer_flag = 1;
					break;

				case (size_t)-1:
					state = state_bak;
					column++;
					break;

				default:
					wc_width = wcwidth(wc);
					if (0 < wc_width)
						column += wc_width;
				}
			} else
#endif	/* HAVE_WIDECHAR */
			{
				if (isprint(c)) {
					if (ctl->graphics_mode && c != 'm')
						/* nothing */ ;
					else {
						ctl->graphics_mode = 0;
						column++;
					}
				} else if (c == ESC) {
					int next = more_getc(ctl, f);
					if (next == '[') {	/* select graphic rendition, ends to 'm' */
						ctl->graphics_mode = 1;
						column--;
					}
					more_ungetc(ctl, next, f);
				}
			}
		}

		if (ctl->num_columns <= column && ctl->fold_opt)
			break;
#ifdef HAVE_WIDECHAR
		if (use_mbc_buffer_flag == 0 && (&ctl->linebuf[ctl->linesz - 1 - 4]) <= p)
			/* don't read another char if there is no space for
			 * whole multibyte sequence */
			break;
#endif
		c = more_getc(ctl, f);
	}
	if (0 < ctl->num_columns && ctl->num_columns <= column)
		if (!ctl->wrap_margin)
			*p++ = '\n';
	colflg = column == ctl->num_columns && ctl->fold_opt;
	if (colflg && ctl->eatnl && ctl->wrap_margin)
		*p++ = '\n';	/* simulate normal wrap */
	*length = p - ctl->linebuf;
	*p = 0;
	return column;
}

/* Most commonly used to erase prompt  */
static void erase_line(struct more_control *ctl)
{
	if (ctl->promptlen == 0)
		return;
	if (ctl->clreol_opt)
		puts(ctl->eraseln);
	else if (ctl->hard_term)
		putchar('\n');
	else {
		putchar('\r');
		if (!ctl->dumb && ctl->eraseln)
			putp(ctl->eraseln);
		else {
			printf("%*s", ctl->promptlen, "");
			putchar('\r');
		}
	}
	ctl->promptlen = 0;
}

#ifdef HAVE_WIDECHAR
static UL_ASAN_BLACKLIST size_t xmbrtowc(const char *s, size_t n)
{
	wchar_t wc;
	mbstate_t mbstate;
	const size_t mblength = mbrtowc(&wc, s, n, &mbstate);
	if (mblength == (size_t)-2 || mblength == (size_t)-1)
		return 1;
	return mblength;
}
#else
/* compiler should remove this function as result of optimization */
static __attribute__((const))size_t
xmbrtowc(const char *s __attribute__((__unused__)),
	 const size_t n __attribute__((__unused__)))
{
	return 1;
}
#endif

static int underline_chars(char *s)
{
	if (s[0] == '_' && s[1] == '\b')
		return 1;
	if (s[0] == '\b' && s[1] == '_')
		return 1;
	return 0;
}

static int bold_chars(char *s)
{
	if (s[0] == s[2] && s[1] == '\b')
		return 1;
	return 0;
}

/* Print a buffer of n characters */
static void print_buffer(struct more_control *ctl, char *s, int n)
{
	enum {
		HL_OFF = 0,
		HL_ON,
		HL_END
	};
	int within_ul = HL_OFF;
	int within_bold = HL_OFF;

	if (!ctl->ul_opt || (memchr(s, '\b', n) == NULL)) {
		fwrite(s, sizeof(char), n, stdout);
		return;
	}
	while (0 < n--) {
		if (2 < n && underline_chars(s)) {
			n -= 2;
			s += 2;
			if (within_ul == HL_OFF) {
				putp(ctl->underline_enter);
				within_ul = HL_ON;
			}
			if (n < 2 || (2 < n && !underline_chars(s + xmbrtowc(s, n))))
				within_ul = HL_END;
		}
		if (3 < n && bold_chars(s)) {
			n -= 2;
			s += 2;
			if (within_bold == HL_OFF) {
				putp(ctl->bold_enter);
				within_bold = HL_ON;
			}
			if (n < 3 || (3 < n && !bold_chars(s + xmbrtowc(s, n))))
				within_bold = HL_END;
		}
		putchar(*s);
		if (within_ul == HL_END || n == 0 || *s == '\n') {
			putp(ctl->underline_exit);
			within_ul = HL_OFF;
		}
		if (within_bold == HL_END || n == 0 || *s == '\n') {
			putp(ctl->bold_exit);
			within_bold = HL_OFF;
		}
		s++;
	}
}

static void output_prompt(struct more_control *ctl, char *filename)
{
	if (0 < ctl->promptlen)
		erase_line(ctl);
	if (!ctl->hard_term) {
		if (ctl->std_enter) {
			putp(ctl->std_enter);
			ctl->promptlen += (2 * ctl->stdout_glitch);
		}
		ctl->promptlen += printf(_("--More--"));
		if (filename != NULL)
			ctl->promptlen += printf(_("(Next file: %s)"), filename);
		else if (!ctl->no_intty) {
			ctl->promptlen +=
			    printf("(%d%%)",
				   (int)((ctl->file_pos * 100) / ctl->file_size));
		}
		if (ctl->no_bell) {
			ctl->promptlen +=
			    printf(_("[Press space to continue, 'q' to quit.]"));
		}
		if (ctl->std_exit)
			putp(ctl->std_exit);
	} else
		fputc('\a', stderr);
	fflush(NULL);
}

static void reset_tty(struct more_control *ctl)
{
	if (ctl->no_tty)
		return;
	fflush(NULL);
	ctl->output_tty.c_lflag |= ICANON | ECHO;
	ctl->output_tty.c_cc[VMIN] = ctl->orig_tty.c_cc[VMIN];
	ctl->output_tty.c_cc[VTIME] = ctl->orig_tty.c_cc[VTIME];
	tcsetattr(STDERR_FILENO, TCSANOW, &ctl->orig_tty);
}

/* Clean up terminal state and exit. Also come here if interrupt signal received */
static void __attribute__((__noreturn__)) exit_more(struct more_control *ctl)
{
#ifdef HAVE_MAGIC
	magic_close(ctl->magic);
#endif
	erase_line(ctl);
	reset_tty(ctl);
	del_curterm(cur_term);
	free(ctl->previousre);
	free(ctl->linebuf);
	free(ctl->go_home);
	exit(EXIT_SUCCESS);
}

static cc_t read_char(struct more_control *ctl)
{
	cc_t c = 0;

	errno = 0;
	if (read(STDERR_FILENO, &c, sizeof(char)) <= 0) {
		if (errno != EINTR)
			exit_more(ctl);
		else
			c = ctl->output_tty.c_cc[VKILL];
	}
	return c;
}

/* Read a decimal number from the terminal.  Set cmd to the non-digit
 * which terminates the number. */
static int read_number(struct more_control *ctl, char *cmd)
{
	int i = 0;
	cc_t ch;

	for (;;) {
		ch = read_char(ctl);
		if (isdigit(ch))
			i = i * 10 + ch - '0';
		else if (ch == ctl->output_tty.c_cc[VKILL])
			i = 0;
		else {
			*cmd = ch;
			break;
		}
	}
	return i;
}

/* Skip nskip files in the file list (from the command line).  Nskip may
 * be negative. */
static void change_file(struct more_control *ctl, int nskip)
{
	if (nskip == 0)
		return;
	if (0 < nskip) {
		if ((ctl->num_files - 1) < (ctl->argv_position + nskip))
			nskip = ctl->num_files - ctl->argv_position - 1;
	}
	ctl->argv_position += nskip;
	if (ctl->argv_position < 0)
		ctl->argv_position = 0;
	if (!ctl->clreol_opt)
		putchar('\n');
	puts(_("...Skipping "));
	if (0 < nskip)
		fputs(_("...Skipping to file "), stdout);
	else
		fputs(_("...Skipping back to file "), stdout);
	puts(ctl->file_names[ctl->argv_position]);
	if (!ctl->clreol_opt)
		putchar('\n');
	ctl->argv_position--;
}

static void show(struct more_control *ctl, char c)
{
	if ((c < ' ' && c != '\n' && c != ESC) || c == CERASE) {
		c += (c == CERASE) ? -0100 : 0100;
		fputs("^", stderr);
		ctl->promptlen++;
	}
	fputc(c, stderr);
	ctl->promptlen++;
}

static void more_error(struct more_control *ctl, char *message)
{
	erase_line(ctl);
	ctl->promptlen += strlen(message);
	if (ctl->std_enter)
		putp(ctl->std_enter);
	putp(message);
	if (ctl->std_exit)
		putp(ctl->std_exit);
	fflush(stdout);
	ctl->errors = 1;
}

static void erase_one_column(struct more_control *ctl)
{
	if (ctl->docrterase)
		fputs("\b ", stderr);
	fputs("\b", stderr);
}

static void ttyin(struct more_control *ctl, char buf[], int nmax, char pchar)
{
	char *sp = buf;
	cc_t c;
	int slash = 0;
	int maxlen = 0;

	while (sp - buf < nmax) {
		if (maxlen < ctl->promptlen)
			maxlen = ctl->promptlen;
		c = read_char(ctl);
		if (c == '\\')
			slash = 1;
		else if (c == ctl->output_tty.c_cc[VERASE] && !slash) {
			if (buf < sp) {
#ifdef HAVE_WIDECHAR
				if (1 < MB_CUR_MAX) {
					wchar_t wc;
					size_t pos = 0, mblength;
					mbstate_t state, state_bak;

					memset(&state, 0, sizeof state);
					while (1) {
						state_bak = state;
						mblength =
						    mbrtowc(&wc, buf + pos,
							    sp - buf, &state);

						state = (mblength == (size_t)-2
							 || mblength ==
							 (size_t)-1) ? state_bak
						    : state;
						mblength =
						    (mblength == (size_t)-2
						     || mblength == (size_t)-1
						     || mblength ==
						     0) ? 1 : mblength;

						if (sp <= (buf + pos + mblength))
							break;

						pos += mblength;
					}

					if (mblength == 1)
						erase_one_column(ctl);
					else {
						int wc_width;
						wc_width = wcwidth(wc);
						wc_width =
						    (wc_width <
						     1) ? 1 : wc_width;
						while (wc_width--)
							erase_one_column(ctl);
					}

					while (mblength--) {
						ctl->promptlen--;
						--sp;
					}
				} else
#endif	/* HAVE_WIDECHAR */
				{
					ctl->promptlen--;
					erase_one_column(ctl);
					sp--;
				}

				if ((*sp < ' ' && *sp != '\n') || *sp == CERASE) {
					ctl->promptlen--;
					erase_one_column(ctl);
				}
				continue;
			} else {
				if (!ctl->eraseln)
					ctl->promptlen = maxlen;
			}
		} else if (( c == ctl->output_tty.c_cc[VKILL]) && !slash) {
			if (ctl->hard_term) {
				show(ctl, c);
				putchar('\n');
				putchar(pchar);
			} else {
				erase_line(ctl);
				putchar(pchar);
				ctl->promptlen = 1;
			}
			sp = buf;
			fflush(stdout);
			continue;
		}
		if (slash && (c == ctl->output_tty.c_cc[VKILL]
			      || c == ctl->output_tty.c_cc[VERASE])) {
			erase_one_column(ctl);
			sp--;
		}
		if (c != '\\')
			slash = 0;
		*sp++ = c;
		if ((c < ' ' && c != '\n' && c != ESC) || c == CERASE) {
			c += (c == CERASE) ? -0100 : 0100;
			fputs("^", stderr);
			ctl->promptlen++;
		}
		if (c != '\n' && c != ESC) {
			fputc(c, stderr);
			ctl->promptlen++;
		} else
			break;
	}
	*--sp = '\0';
	if (!ctl->eraseln)
		ctl->promptlen = maxlen;
	if (nmax < (sp - buf))
		more_error(ctl, _("Line too long"));
}

static int command_expansion(struct more_control *ctl, char **outbuf, char *inbuf)
{
	char *inpstr = inbuf;
	char *allocation;
	char *outstr;
	char c;
	int changed = 0;
	size_t xtra;
	ptrdiff_t offset;

	if (!strpbrk(inbuf, "%!\\")) {
		*outbuf = xstrdup(inbuf);
		return 0;
	}
	xtra = strlen(ctl->file_names[ctl->argv_position]) + strlen(ctl->shell_line) + COMMAND_BUF + 1;
	outstr = allocation = xmalloc(xtra);
	while ((c = *inpstr++) != '\0') {
		offset = outstr - allocation;
		if ((xtra - offset - 1) < xtra) {
			xtra += COMMAND_BUF;
			allocation = xrealloc(allocation, xtra);
			outstr = allocation + offset;
		}
		switch (c) {
		case '%':
			if (!ctl->no_intty) {
				strcpy(outstr, ctl->file_names[ctl->argv_position]);
				outstr += strlen(ctl->file_names[ctl->argv_position]);
				changed = 1;
			} else
				*outstr++ = c;
			break;
		case '!':
			if (!ctl->shellp)
				more_error(ctl, _
					   ("No previous command to substitute for"));
			strcpy(outstr, ctl->shell_line);
			outstr += strlen(ctl->shell_line);
			changed = 1;
			break;
		case '\\':
			if (*inpstr == '%' || *inpstr == '!') {
				*outstr++ = *inpstr++;
				break;
			}
		default:
			*outstr++ = c;
		}
	}
	*outstr++ = '\0';
	*outbuf = allocation;
	return changed;
}

static void set_tty(struct more_control *ctl)
{
	ctl->output_tty.c_lflag &= ~(ICANON | ECHO);
	ctl->output_tty.c_cc[VMIN] = 1;	/* read at least 1 char */
	ctl->output_tty.c_cc[VTIME] = 0;	/* no timeout */
	tcsetattr(STDERR_FILENO, TCSANOW, &ctl->output_tty);
}

/* Come here if a quit signal is received */
static void quit_more(struct more_control *ctl)
{
	if (!ctl->no_bell && ctl->notell) {
		ctl->promptlen += fprintf(stderr, _("[Use q or Q to quit]"));
		ctl->notell = 0;
	} else
		exit_more(ctl);
}

/* Come here when we get a suspend signal from the terminal */
static void suspend_more(struct more_control *ctl)
{
	reset_tty(ctl);
	kill(getpid(), SIGSTOP);
	/* We're back */
	set_tty(ctl);
}

static void free_args(char ***args)
{
	free(*args);
}

static void execute(struct more_control *ctl, char *filename, char *cmd, ...)
{
	int id;
	va_list argp;
	char *arg;
	char **args  __attribute__((__cleanup__(free_args))) = NULL;
	int argcount;

	fflush(NULL);
	if ((id = fork()) < 0) {
		warn(_("fork failed"));
		goto err;
	}
	if (id == 0) {
		if (!isatty(STDIN_FILENO)) {
			close(STDIN_FILENO);
			if (open("/dev/tty", 0) < 0) {
				fprintf(stderr, _("cannot open %s"), "/dev/tty\n");
				goto err;
			}
		}
		reset_tty(ctl);

		va_start(argp, cmd);
		arg = va_arg(argp, char *);
		argcount = 0;
		while (arg) {
			argcount++;
			arg = va_arg(argp, char *);
		}
		va_end(argp);

		args = xmalloc(sizeof(char *) * (argcount + 1));
		args[argcount] = NULL;

		va_start(argp, cmd);
		arg = va_arg(argp, char *);
		argcount = 0;
		while (arg) {
			args[argcount] = arg;
			argcount++;
			arg = va_arg(argp, char *);
		}
		va_end(argp);

		if (geteuid() != getuid()) {
			/* in case someone uses more(1) as setuid binary */
			if (setuid(getuid()) < 0)
				err(EXIT_FAILURE, _("setuid failed"));
			if (setgid(getgid()) < 0)
				err(EXIT_FAILURE, _("setgid failed"));
		}
		execvp(cmd, args);
		fputs(_("exec failed\n"), stderr);
		exit(EXIT_FAILURE);
	}
	if (0 < id) {
		while (0 < wait(0))
			/* nothing */ ;
	} else
		fputs(_("can't fork\n"), stderr);
	set_tty(ctl);
	puts("------------------------");
err:
	output_prompt(ctl, filename);
}

static void do_shell(struct more_control *ctl, char *filename)
{
	char cmdbuf[COMMAND_BUF];
	int rc;
	char *expanded = NULL;

	erase_line(ctl);
	putchar('!');
	fflush(stdout);
	ctl->promptlen = 1;
	if (ctl->rerun_command)
		fputs(ctl->shell_line, stdout);
	else {
		ttyin(ctl, cmdbuf, sizeof(cmdbuf) - 2, '!');
		rc = command_expansion(ctl, &expanded, cmdbuf);
		if (expanded) {
			if (strlen(expanded) < (sizeof(ctl->shell_line) - 1))
				xstrncpy(ctl->shell_line, expanded, sizeof ctl->shell_line);
			else
				rc = -1;
			free(expanded);
		}
		if (rc < 0) {
			fputs(_("  Overflow\n"), stderr);
			output_prompt(ctl, filename);
			return;
		} else if (0 < rc) {
			erase_line(ctl);
			ctl->promptlen = printf("!%s", ctl->shell_line);
		}
	}
	fputc('\n', stderr);
	fflush(NULL);
	ctl->promptlen = 0;
	ctl->shellp = 1;
	execute(ctl, filename, ctl->shell, ctl->shell, "-c", ctl->shell_line, 0);
}

/* Execute a colon-prefixed command.  Returns <0 if not a command that
 * should cause more of the file to be printed. */
static int colon_command(struct more_control *ctl, char *filename, int cmd, int nlines)
{
	cc_t ch;

	if (cmd == 0)
		ch = read_char(ctl);
	else
		ch = cmd;
	ctl->lastcolon = ch;
	switch (ch) {
	case 'f':
		erase_line(ctl);
		if (!ctl->no_intty)
			ctl->promptlen =
			    printf(_("\"%s\" line %jd"), ctl->file_names[ctl->argv_position],
				     (intmax_t) ctl->current_line);
		else
			ctl->promptlen = printf(_("[Not a file] line %jd"),
						  (intmax_t) ctl->current_line);
		fflush(stdout);
		return -1;
	case 'n':
		if (nlines == 0) {
			if ((ctl->num_files - 1) <= ctl->argv_position)
				exit_more(ctl);
			nlines++;
		}
		erase_line(ctl);
		change_file(ctl, nlines);
		return 0;
	case 'p':
		if (ctl->no_intty) {
			fputc('\a', stderr);
			return -1;
		}
		erase_line(ctl);
		if (nlines == 0)
			nlines++;
		change_file(ctl, -nlines);
		return 0;
	case '!':
		do_shell(ctl, filename);
		return -1;
	case 'q':
	case 'Q':
		exit_more(ctl);
	default:
		fputc('\a', stderr);
		return -1;
	}
}

/* Skip n lines in the file f */
static void skip_lines(struct more_control *ctl, FILE *f)
{
	int c, n = ctl->jump_len;

	while (0 < n) {
		while ((c = more_getc(ctl, f)) != '\n')
			if (c == EOF)
				return;
		n--;
		ctl->current_line++;
	}
}

/*  Clear the screen */
static void clear_tty(struct more_control *ctl)
{
	if (ctl->clear && !ctl->hard_term) {
		putp(ctl->clear);
		/* Put out carriage return so that system doesn't get
		 * confused by escape sequences when expanding tabs */
		ctl->promptlen = 0;
	}
}

static void read_line(struct more_control *ctl, FILE *f)
{
	int c;
	char *p;

	p = ctl->linebuf;
	while ((c = more_getc(ctl, f)) != '\n' && c != EOF
	       && (size_t)(p - ctl->linebuf) < ctl->linesz - 1)
		*p++ = c;
	if (c == '\n')
		ctl->current_line++;
	*p = '\0';
}

volatile sig_atomic_t alarm_received;

static void sig_alarm_handler(int sig __attribute__((__unused__)))
{
	alarm_received = 1;
}

static int stop_search(struct more_control *ctl)
{
	char buf[2];

	ctl->promptlen = printf(_("No search results in %d seconds, stop searching?"), SEARCH_TIMEOUT);
	buf[0] = getchar();
	buf[1] = '\0';
	erase_line(ctl);
	alarm(SEARCH_TIMEOUT);
	alarm_received = 0;
	return rpmatch(buf);
}

/* Search for nth occurrence of regular expression contained in buf in
 * the file */
static void search(struct more_control *ctl, char buf[], FILE *file, int n)
{
	off_t startline = ctl->file_pos;
	off_t line1 = startline;
	off_t line2 = startline;
	off_t line3;
	int lncount = 0;
	int saveln, rc;
	int flags = REG_NOSUB;
	regex_t re;

	ctl->context.line_num = saveln = ctl->current_line;
	ctl->context.row_num = startline;
	if (ctl->ignore_case_opt)
		flags |= REG_ICASE;
	if (!buf)
		goto notfound;
	if ((rc = regcomp(&re, buf, flags)) != 0) {
		char s[REGERR_BUF];
		regerror(rc, &re, s, sizeof s);
		more_error(ctl, s);
		return;
	}
	alarm_received = 0;
	signal(SIGALRM, sig_alarm_handler);
	alarm(SEARCH_TIMEOUT);
	while (!feof(file)) {
		if (alarm_received && (stop_search(ctl) == RPMATCH_YES))
			break;
		line3 = line2;
		line2 = line1;
		line1 = ctl->file_pos;
		read_line(ctl, file);
		lncount++;
		if (regexec(&re, ctl->linebuf, 0, NULL, 0) == 0) {
			if (--n == 0) {
				if ((1 < lncount && ctl->no_intty) || 3 < lncount) {
					putchar('\n');
					if (ctl->clreol_opt)
						putp(ctl->eraseln);
					fputs(_("...skipping\n"), stdout);
				}
				if (!ctl->no_intty) {
					ctl->current_line -=
					    (lncount < 3 ? lncount : 3);
					more_fseek(ctl, file, line3);
					if (ctl->noscroll_opt) {
						if (ctl->clreol_opt) {
							tputs(ctl->go_home,
							      STDOUT_FILENO,
							      putchar);
							putp(ctl->eraseln);
						} else
							clear_tty(ctl);
					}
				} else {
					erase_line(ctl);
					if (ctl->noscroll_opt) {
						if (ctl->clreol_opt) {
							tputs(ctl->go_home,
							      STDOUT_FILENO,
							      putchar);
							putp(ctl->eraseln);
						} else
							clear_tty(ctl);
					}
					puts(ctl->linebuf);
				}
				break;
			}
		}
	}
	alarm(0);
	signal(SIGALRM, SIG_DFL);
	regfree(&re);
	if (feof(file)) {
		if (!ctl->no_intty) {
			ctl->current_line = saveln;
			more_fseek(ctl, file, startline);
		} else {
			fputs(_("\nPattern not found\n"), stdout);
			exit_more(ctl);
		}
		free(ctl->previousre);
		ctl->previousre = NULL;
notfound:
		more_error(ctl, _("Pattern not found"));
	}
}

static char *find_editor(void)
{
	char *editor;

	editor = getenv("VISUAL");
	if (editor == NULL || *editor == '\0')
		editor = getenv("EDITOR");
	if (editor == NULL || *editor == '\0')
		editor = _PATH_VI;
	return editor;
}

static void runtime_usage(void)
{
	const char separator[] = "-------------------------------------------------------------------------------\n";
	fputs(separator, stdout);
	fputs(_("Most commands optionally preceded by integer argument k.  "
		"Defaults in brackets.\n"
		"Star (*) indicates argument becomes new default.\n"), stdout);
	fputs(_("<space>         display next k lines of text [current screen size]\n"), stdout);
	fputs(_("z               display next k lines of text [current screen size]*\n"), stdout);
	fputs(_("<return>        display next k lines of text [1]*\n"), stdout);
	fputs(_("d or ctrl-D     scroll k lines [current scroll size, initially 11]*\n"), stdout);
	fputs(_("q, Q or ctrl-C  exit from more\n"), stdout);
	fputs(_("s               skip forward k lines of text [1]\n"), stdout);
	fputs(_("f               skip forward k screenfuls of text [1]\n"), stdout);
	fputs(_("b or ctrl-B     skip backwards k screenfuls of text [1]\n"), stdout);
	fputs(_("'               go to place where previous search started\n"), stdout);
	fputs(_("=               display current line number\n"), stdout);
	fputs(_("/<regexp>       search for kth occurrence of regular expression [1]\n"), stdout);
	fputs(_("n               search for kth occurrence of last regexp [1]\n"), stdout);
	fputs(_("!<cmd>          execute <cmd> in a subshell\n"), stdout);
	fprintf(stdout,
	      _("v               start up %s at current line\n"), find_editor());
	fputs(_("ctrl-L          redraw screen\n"), stdout);
	fputs(_(":n              go to kth next file [1]\n"), stdout);
	fputs(_(":p              go to kth previous file [1]\n"), stdout);
	fputs(_(":f              display current file name and line number\n"), stdout);
	fputs(_(".               repeat previous command\n"), stdout);
	fputs(separator, stdout);
}

static void execute_editor(struct more_control *ctl, char *cmdbuf, char *filename)
{
	char *editor, *p;
	int n = (ctl->current_line - ctl->lines_per_screen <= 0 ? 1 : ctl->current_line - (ctl->lines_per_screen + 1) / 2);
	int split = 0;

	editor = find_editor();
	p = strrchr(editor, '/');
	if (p)
		p++;
	else
		p = editor;
	/* Earlier: call vi +n file.  This also works for emacs.  POSIX:
	 * call vi -c n file (when editor is vi or ex).  */
	if (!strcmp(p, "vi") || !strcmp(p, "ex")) {
		sprintf(cmdbuf, "-c %d", n);
		split = 1;
	} else
		sprintf(cmdbuf, "+%d", n);
	erase_line(ctl);
	printf("%s %s %s", editor, cmdbuf, ctl->file_names[ctl->argv_position]);
	if (split) {
		cmdbuf[2] = 0;
		execute(ctl, filename, editor, editor,
			cmdbuf, cmdbuf + 3, ctl->file_names[ctl->argv_position], (char *)0);
	} else
		execute(ctl, filename, editor, editor, cmdbuf, ctl->file_names[ctl->argv_position], (char *)0);
}

static int skip_backwards(struct more_control *ctl, FILE *f, int nlines)
{
	if (nlines == 0)
		nlines++;
	erase_line(ctl);
	printf(P_("...back %d page", "...back %d pages", nlines), nlines);
	putchar('\n');
	ctl->jump_len = ctl->current_line - (ctl->lines_per_screen * (nlines + 1));
	if (!ctl->noscroll_opt)
		ctl->jump_len--;
	more_fseek(ctl, f, 0);
	ctl->current_line = 0;
	if (ctl->jump_len < 1)
		ctl->jump_len = 0;
	else
		skip_lines(ctl, f);
	return ctl->lines_per_screen;
}

static int skip_forwards(struct more_control *ctl, FILE *f, int nlines, char comchar)
{
	if (nlines == 0)
		nlines++;
	if (comchar == 'f')
		nlines *= ctl->lines_per_screen;
	erase_line(ctl);
	printf(P_("...skipping %d line", "...skipping %d lines", nlines), nlines);
	putchar('\n');
	ctl->jump_len = nlines;
	skip_lines(ctl, f);
	return 1;
}

/* Come here if a signal for a window size change is received */
#ifdef SIGWINCH
static void change_window_sz(struct more_control *ctl)
{
	struct winsize win;

	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &win) != -1) {
		if (win.ws_row != 0) {
			ctl->lines_per_page = win.ws_row;
			ctl->d_scroll_len = ctl->lines_per_page / 2 - 1;
			if (ctl->d_scroll_len <= 0)
				ctl->d_scroll_len = 1;
			ctl->lines_per_screen = ctl->lines_per_page - 1;
		}
		if (win.ws_col != 0)
			ctl->num_columns = win.ws_col;
	}
	prepare_line_buffer(ctl);
}
#endif				/* SIGWINCH */

/* Read a command and do it.  A command consists of an optional integer
 * argument followed by the command character.  Return the number of
 * lines to display in the next screenful.  If there is nothing more to
 * display in the current file, zero is returned. */
static int command(struct more_control *ctl, char *filename, FILE *f)
{
	int nlines;
	int retval = 0;
	char colonch;
	int done = 0;
	char comchar, cmdbuf[INIT_BUF];
	struct pollfd pfd[2];

	pfd[0].fd = ctl->sigfd;
	pfd[0].events = POLLIN | POLLERR | POLLHUP;
	pfd[1].fd = STDIN_FILENO;
	pfd[1].events = POLLIN;
	if (!ctl->errors)
		output_prompt(ctl, filename);
	else
		ctl->errors = 0;
	ctl->search_called = 0;
	while (!done) {
		if (poll(pfd, 2, -1) < 0) {
			if (errno == EAGAIN)
				continue;
			more_error(ctl, _("poll failed"));
			continue;
		}
		if (pfd[0].revents != 0) {
			struct signalfd_siginfo info;
			ssize_t sz;

			sz = read(pfd[0].fd, &info, sizeof(info));
			assert(sz == sizeof(info));
			switch (info.ssi_signo) {
			case SIGINT:
				exit_more(ctl);
				break;
			case SIGQUIT:
				quit_more(ctl);
				break;
			case SIGTSTP:
				suspend_more(ctl);
				break;
#ifdef SIGWINCH
			case SIGWINCH:
				change_window_sz(ctl);
				break;
#endif
			default:
				abort();
			}
		}
		if (pfd[1].revents == 0)
			continue;
		nlines = read_number(ctl, &comchar);
		ctl->rerun_command = colonch = 0;
		if (comchar == '.') {	/* Repeat last command */
			ctl->rerun_command++;
			comchar = ctl->lastcmd;
			nlines = ctl->lastarg;
			if (ctl->lastcmd == ':')
				colonch = ctl->lastcolon;
		}
		ctl->lastcmd = comchar;
		ctl->lastarg = nlines;
		if ((cc_t) comchar == ctl->output_tty.c_cc[VERASE]) {
			erase_line(ctl);
			output_prompt(ctl, filename);
			continue;
		}
		switch (comchar) {
		case ':':
			retval = colon_command(ctl, filename, colonch, nlines);
			if (0 <= retval)
				done++;
			break;
		case 'b':
		case CTRL('B'):
			if (ctl->no_intty) {
				fputc('\a', stderr);
				return -1;
			}
			retval = skip_backwards(ctl, f, nlines);
			done = 1;
			break;
		case ' ':
		case 'z':
			if (nlines == 0)
				nlines = ctl->lines_per_screen;
			else if (comchar == 'z')
				ctl->lines_per_screen = nlines;
			retval = nlines;
			done = 1;
			break;
		case 'd':
		case CTRL('D'):
			if (nlines != 0)
				ctl->d_scroll_len = nlines;
			retval = ctl->d_scroll_len;
			done = 1;
			break;
		case 'q':
		case 'Q':
			exit_more(ctl);
		case 's':
		case 'f':
		case CTRL('F'):
			if (skip_forwards(ctl, f, nlines, comchar))
				retval = ctl->lines_per_screen;
			else
				retval = 0;
			done = 1;
			break;
		case '\n':
			if (nlines != 0)
				ctl->lines_per_screen = nlines;
			else
				nlines = 1;
			retval = nlines;
			done = 1;
			break;
		case '\f':
			if (!ctl->no_intty) {
				clear_tty(ctl);
				more_fseek(ctl, f, ctl->screen_start.row_num);
				ctl->current_line = ctl->screen_start.line_num;
				retval = ctl->lines_per_screen;
				done = 1;
				break;
			}
			fputc('\a', stderr);
			break;
		case '\'':
			if (!ctl->no_intty) {
				erase_line(ctl);
				fputs(_("\n***Back***\n\n"), stdout);
				more_fseek(ctl, f, ctl->context.row_num);
				ctl->current_line = ctl->context.line_num;
				retval = lines;
				done = 1;
				break;
			}
			fputc('\a', stderr);
			break;
		case '=':
			erase_line(ctl);
			ctl->promptlen = printf("%jd", (intmax_t) ctl->current_line);
			fflush(stdout);
			break;
		case 'n':
			if (!ctl->previousre) {
				more_error(ctl, _("No previous regular expression"));
				break;
			}
			ctl->rerun_command = 1;
			/* fall through */
		case '/':
			ctl->search_called = 1;
			if (nlines == 0)
				nlines++;
			erase_line(ctl);
			putchar('/');
			ctl->promptlen = 1;
			fflush(stdout);
			if (ctl->rerun_command) {
				fputc('\r', stderr);
				search(ctl, ctl->previousre, f, nlines);
			} else {
				ttyin(ctl, cmdbuf, sizeof(cmdbuf) - 2, '/');
				fputc('\r', stderr);
				free(ctl->previousre);
				ctl->previousre = xstrdup(cmdbuf);
				search(ctl, cmdbuf, f, nlines);
			}
			retval = ctl->lines_per_screen - 1;
			done = 1;
			break;
		case '!':
			do_shell(ctl, filename);
			break;
		case '?':
		case 'h':
			erase_line(ctl);
			if (ctl->noscroll_opt)
				clear_tty(ctl);
			runtime_usage();
			output_prompt(ctl, filename);
			break;
		case 'v':	/* This case should go right before default */
			if (!ctl->no_intty) {
				execute_editor(ctl, cmdbuf, filename);
				break;
			}
			/* fall through */
		default:
			if (ctl->no_bell) {
				erase_line(ctl);
				if (ctl->std_enter)
					putp(ctl->std_enter);
				ctl->promptlen =
				    printf(_("[Press 'h' for instructions.]")) +
				    2 * ctl->stdout_glitch;
				if (ctl->std_exit)
					putp(ctl->std_exit);
				fflush(stdout);
			} else
				fputc('\a', stderr);
			break;
		}
	}
	putchar('\r');
	ctl->notell = 1;
	return retval;
}

/* Print out the contents of the file f, one screenful at a time. */
static void screen(struct more_control *ctl, FILE *f, int num_lines)
{
	int c;
	int nchars;
	int length;			/* length of current line */
	static int prev_len = 1;	/* length of previous line */

	for (;;) {
		while (0 < num_lines && !ctl->is_paused) {
			if ((nchars = get_line(ctl, f, &length)) == EOF) {
				if (ctl->clreol_opt)
					tputs(ctl->end_clear, STDOUT_FILENO, putchar);
				return;
			}
			if (ctl->squeeze_opt && length == 0 && prev_len == 0)
				continue;
			prev_len = length;
			erase_line(ctl);
			print_buffer(ctl, ctl->linebuf, length);
			if (ctl->clreol_opt)
				tputs(ctl->eraseln, STDOUT_FILENO, putchar);
			if (nchars < ctl->num_columns || !ctl->fold_opt)
				print_buffer(ctl, "\n", 1);	/* will turn off UL if necessary */
			num_lines--;
		}
		fflush(stdout);
		if ((c = more_getc(ctl, f)) == EOF) {
			if (ctl->clreol_opt)
				tputs(ctl->end_clear, STDOUT_FILENO, putchar);
			return;
		}

		if (ctl->is_paused && ctl->clreol_opt)
			tputs(ctl->end_clear, STDOUT_FILENO, putchar);
		more_ungetc(ctl, c, f);
		ctl->is_paused = 0;
		do {
			if ((num_lines = command(ctl, NULL, f)) == 0)
				return;
		} while (ctl->search_called && !ctl->previousre);
		if (ctl->clreol_opt)
			printf("\b \b");
		if (ctl->hard_term && 0 < ctl->promptlen)
			erase_line(ctl);
		if (ctl->noscroll_opt && ctl->lines_per_screen <= num_lines) {
			if (ctl->clreol_opt)
				tputs(ctl->go_home, STDOUT_FILENO, putchar);
			else
				clear_tty(ctl);
		}
		ctl->screen_start.line_num = ctl->current_line;
		ctl->screen_start.row_num = ctl->file_pos;
	}
}

static void copy_file(FILE *f)
{
	char buf[BUFSIZ];
	size_t sz;

	while (0 < (sz = fread(&buf, sizeof(char), sizeof(buf), f)))
		fwrite(&buf, sizeof(char), sz, stdout);
}

static void initterm(struct more_control *ctl)
{
	int ret;
	char *term;
	struct winsize win;
	const char *cursorm;

#ifndef NON_INTERACTIVE_MORE
	ctl->no_tty = (tcgetattr(STDOUT_FILENO, &ctl->output_tty) != 0);
#endif
	ctl->no_intty = (tcgetattr(STDIN_FILENO, &ctl->output_tty) != 0);
	tcgetattr(STDERR_FILENO, &ctl->output_tty);
	ctl->orig_tty = ctl->output_tty;
	ctl->hardtabs = (ctl->output_tty.c_oflag & TABDLY) != TAB3;
	if (!ctl->no_tty) {
		ctl->output_tty.c_lflag &= ~(ICANON | ECHO);
		ctl->output_tty.c_cc[VMIN] = 1;
		ctl->output_tty.c_cc[VTIME] = 0;
	}
	if (ctl->no_tty)
		return;
	ctl->docrterase = (ctl->output_tty.c_cc[VERASE] != 255);
	if ((term = getenv("TERM")) == NULL) {
		ctl->dumb = 1;
		ctl->ul_opt = 0;
	}
	if ((ctl->shell = getenv("SHELL")) == NULL)
		ctl->shell = _PATH_BSHELL;
	setupterm(term, STDOUT_FILENO, &ret);
	if (ret < 1) {
		ctl->dumb = 1;
		ctl->ul_opt = 0;
		return;
	}
	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &win) < 0) {
		ctl->lines_per_page = tigetnum(TERM_LINES);
		ctl->num_columns = tigetnum(TERM_COLS);
	} else {
		if ((ctl->lines_per_page = win.ws_row) == 0)
			ctl->lines_per_page = tigetnum(TERM_LINES);
		if ((ctl->num_columns = win.ws_col) == 0)
			ctl->num_columns = tigetnum(TERM_COLS);
	}
	if ((ctl->lines_per_page < 1) || tigetflag(TERM_HARD_COPY)) {
		ctl->hard_term = 1;
		ctl->lines_per_page = LINES_PER_PAGE;
	}
	if (ctl->num_columns < 1)
		ctl->num_columns = NUM_COLUMNS;
	if (tigetflag(TERM_EAT_NEW_LINE))
		/* Eat newline at last column + 1; dec, concept */
		ctl->eatnl = 1;
	ctl->wrap_margin = tigetflag(TERM_AUTO_RIGHT_MARGIN);
	ctl->eraseln = tigetstr(TERM_CLEAR_TO_LINE_END);
	ctl->clear = tigetstr(TERM_CLEAR);
	if ((ctl->std_enter = tigetstr(TERM_STANDARD_MODE)) != NULL)
		ctl->std_exit = tigetstr(TERM_EXIT_STANDARD_MODE);
	if (0 < tigetnum(TERM_STD_MODE_GLITCH))
		ctl->stdout_glitch = 1;
	/* Set up for underlining:  some terminals don't need it; others have
	 * start/stop sequences, still others have an underline char sequence
	 * which is assumed to move the cursor forward one character.  If
	 * underline sequence isn't available, settle for standout sequence.
	 */
	if (tigetflag(TERM_UNDERLINE)
	    || tigetflag(TERM_OVER_STRIKE))
		ctl->ul_opt = 0;
	if (((ctl->underline_enter = tigetstr(TERM_ENTER_UNDERLINE)) == NULL)
	     || ((ctl->underline_exit = tigetstr(TERM_EXIT_UNDERLINE)) == NULL)) {
		ctl->underline_enter = "";
		ctl->underline_exit = "";
	}
	if (((ctl->bold_enter = tigetstr(TERM_ENTER_BOLD)) == NULL
	     || (ctl->bold_exit = tigetstr(TERM_EXIT_BOLD)) == NULL)) {
		ctl->bold_enter = "";
		ctl->bold_exit = "";
	}
	cursorm = tigetstr(TERM_HOME);
	if (cursorm == NULL || cursorm == (char *)-1) {
		cursorm = tigetstr(TERM_CURSOR_ADDRESS);
		if (cursorm == NULL || cursorm == (char *)-1)
			cursorm = tparm(cursorm, 0, 0);
	}
	ctl->go_home = xstrdup(cursorm);
	ctl->end_clear = tigetstr(TERM_CLEAR_TO_SCREEN_END);
}

static void display_file(struct more_control *ctl, FILE *f, char *initbuf, int left)
{
	ctl->context.line_num = ctl->context.row_num = 0;
	ctl->current_line = 0;
	ctl->file_pos = 0;
	if (ctl->first_file) {
		ctl->first_file = 0;
		if (ctl->search_opt) {
			free(ctl->previousre);
			ctl->previousre = xstrdup(initbuf);
			search(ctl, initbuf, f, 1);
			if (ctl->noscroll_opt)
				left--;
		} else if (ctl->jump_defined)
			skip_lines(ctl, f);
	} else if (ctl->argv_position < ctl->num_files && !ctl->no_tty)
		left = command(ctl, ctl->file_names[ctl->argv_position], f);
	if (left != 0) {
		if ((ctl->noscroll_opt || ctl->clearfirst)
		    && (ctl->file_size != LONG_MAX)) {
			if (ctl->clreol_opt)
				tputs(ctl->go_home, STDOUT_FILENO, putchar);
			else
				clear_tty(ctl);
		}
		if (ctl->print_names) {
			const char separator[] = "::::::::::::::";
			erase_line(ctl);
			puts(separator);
			erase_line(ctl);
			puts(ctl->file_names[ctl->argv_position]);
			erase_line(ctl);
			puts(separator);
			if ((ctl->lines_per_page - 4) < left)
				left = ctl->lines_per_page - 4;
		}
		if (ctl->no_tty)
			copy_file(f);
		else
			screen(ctl, f, left);
	}
	fflush(NULL);
	fclose(f);
	ctl->screen_start.line_num = ctl->screen_start.row_num = 0L;
	ctl->context.line_num = ctl->context.row_num = 0L;
}

int main(int argc, char **argv)
{
	FILE *f;
	char *s;
	int c;
	int left;
	char *initbuf = NULL;
	sigset_t sigset;
	struct more_control ctl = {
		.first_file = 1,
		.fold_opt = 1,
		.notell = 1,
		.stop_opt = 1,
		.ul_opt = 1,
		.wrap_margin = 1,
		.lines_per_page = LINES_PER_PAGE,
		.num_columns = NUM_COLUMNS,
		.d_scroll_len = SCROLL_LEN,
		0
	};

	setlocale(LC_ALL, "");
	bindtextdomain(PACKAGE, LOCALEDIR);
	textdomain(PACKAGE);
	atexit(close_stdout);

	ctl.num_files = argc;
	ctl.file_names = argv;
	setlocale(LC_ALL, "");
	initterm(&ctl);

	/* Auto set no scroll on when binary is called page */
	if (!(strcmp(program_invocation_short_name, "page")))
		ctl.noscroll_opt = 1;

#ifdef HAVE_MAGIC
	ctl.magic = magic_open(MAGIC_MIME_ENCODING | MAGIC_SYMLINK);
	magic_load(ctl.magic, NULL);
#endif
	prepare_line_buffer(&ctl);

	ctl.d_scroll_len = ctl.lines_per_page / 2 - 1;
	if (ctl.d_scroll_len <= 0)
		ctl.d_scroll_len = 1;

	if ((s = getenv("MORE")) != NULL)
		argscan(&ctl, s);

	while (0 < --ctl.num_files) {
		if ((c = (*++ctl.file_names)[0]) == '-') {
			argscan(&ctl, *ctl.file_names + 1);
		} else if (c == '+') {
			s = *ctl.file_names;
			if (*++s == '/') {
				ctl.search_opt = 1;
				initbuf = xstrdup(s + 1);
			} else {
				ctl.jump_defined = 1;
				for (ctl.jump_len = 0; *s != '\0'; s++)
					if (isdigit(*s))
						ctl.jump_len =
						    ctl.jump_len * 10 + *s - '0';
				ctl.jump_len--;
			}
		} else
			break;
	}
	/* allow clreol only if go_home and eraseln and end_clear strings are
	 * defined, and in that case, make sure we are in noscroll mode */
	if (ctl.clreol_opt) {
		if ((ctl.go_home == NULL) || (*ctl.go_home == '\0') ||
		    (ctl.eraseln == NULL) || (*ctl.eraseln == '\0') ||
		    (ctl.end_clear == NULL) || (*ctl.end_clear == '\0'))
			ctl.clreol_opt = 0;
		else
			ctl.noscroll_opt = 1;
	}
	if (ctl.lines_per_screen == 0)
		ctl.lines_per_screen = ctl.lines_per_page - 1;
	left = ctl.lines_per_screen;
	if (1 < ctl.num_files)
		ctl.print_names = 1;
	if (!ctl.no_intty && ctl.num_files == 0)
		usage(stderr);
	if (!ctl.no_tty)
		tcsetattr(STDERR_FILENO, TCSANOW, &ctl.output_tty);
	sigemptyset(&sigset);
	sigaddset(&sigset, SIGINT);
	sigaddset(&sigset, SIGQUIT);
	sigaddset(&sigset, SIGTSTP);
#ifdef SIGWINCH
	sigaddset(&sigset, SIGWINCH);
#endif
	sigprocmask(SIG_BLOCK, &sigset, NULL);
	ctl.sigfd = signalfd(-1, &sigset, SFD_CLOEXEC);
	if (ctl.no_intty) {
		if (ctl.no_tty)
			copy_file(stdin);
		else {
			f = stdin;
			display_file(&ctl, f, initbuf, left);
		}
		ctl.first_file = 0;
		ctl.no_intty = 0;
		ctl.print_names = 1;
	}

	for (/* nothing */; ctl.argv_position < ctl.num_files; ctl.argv_position++) {
		if ((f = more_fopen(&ctl, ctl.file_names[ctl.argv_position])) == NULL)
			continue;
		display_file(&ctl, f, initbuf, left);
		ctl.first_file = 0;
	}
	exit_more(&ctl);
}
