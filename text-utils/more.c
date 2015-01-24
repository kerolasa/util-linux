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
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#ifdef HAVE_NCURSES_H
# include <ncurses.h>
#elif defined(HAVE_NCURSES_NCURSES_H)
# include <ncurses/ncurses.h>
#endif

#include "more-term.h"		/* include after <curses.h> */

#include "closestream.h"
#include "nls.h"
#include "rpmatch.h"
#include "strutils.h"
#include "widechar.h"
#include "xalloc.h"

#ifdef TEST_PROGRAM
# define NON_INTERACTIVE_MORE 1
#endif

#ifndef XTABS
# define XTABS	TAB3
#endif

#define TBUFSIZ		1024
#define LINSIZ		256	/* minimal Line buffer size */
#define ctrl(letter)	(letter & 077)
#define RUBOUT		'\177'
#define ESC		'\033'
#define QUIT		'\034'
#define SCROLL_LEN	11
#define LINES_PER_PAGE	24
#define NUM_COLUMNS	80
#define TERMINAL_BUF	4096
#define INIT_BUF	80
#define SHELL_LINE	1000
#define COMMAND_BUF	200
#define REGERR_BUF	NUM_COLUMNS
#define STOP		-10
#define SEARCH_TIMEOUT	10

struct more_control {
	struct termios otty;		/* output terminal */
	struct termios orig_tty;	/* original terminal settings */
	long file_pos;			/* file position */
	long file_size;			/* file size */
	int argv_position;		/* position in argv[] */
	int nscroll;			/* number of lines scrolled by 'd' */
	int lines_per_screen;		/* screen size in lines */
	int promptlen;			/* message prompt length */
	int jumpline;			/* line number to jump */
	int current_line;		/* line we are currently at */
	char **fnames;			/* the list of file names */
	int nfiles;			/* number of files left to process */
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
	char *underlining_char;		/* underline character */
	char *backspace_char;		/* backspace character */
	char *go_home;			/* go to screen home position */
	char *cursorm;			/* cursor move destination */
	char cursorhome[40];		/* contains cursor movement to home */
	char *end_clear;		/* clear rest of screen */
	int ncolumns;			/* number of columns */
	char *previousre;		/* previous search() buf[] item */
	struct {
		long nrow;		/* row number */
		long line;		/* line number */
	} context,
	  screen_start;
	int lastcmd;			/* previous more key command */
	int lastarg;			/* previous key command argument */
	int lastcolon;			/* is a colon-prefixed key command */
	char shell_line[SHELL_LINE];
#if !defined(HAVE_NCURSES_H) && !defined(HAVE_NCURSES_NCURSES_H) && defined(HAVE_LIBTERMCAP)
	char tcbuffer[TERMINAL_BUF];	/* buffer for my_setupterm() in more-term.h */
#endif
	unsigned int
		bad_so:1,		/* true if overwriting does not turn off standout */
		catch_susp:1,		/* should SIGTSTP signal be caught */
		clearfirst:1,		/* is first character in file \f */
		clreol_opt:1,		/* do not scroll, paint each screen from the top */
		docrterase:1,		/* is erase previous supported */
		docrtkill:1,		/* is erase input supported */
		dumb:1,			/* is terminal type known */
		eatnl:1,		/* is newline ignored after 80 cols */
		errors:1,		/* is an error reported */
		first_file:1,		/* is the input file the first in list */
		fold_opt:1,		/* fold long lines */
		hard_term:1,		/* is this hard copy terminal */
		hardtabs:1,		/* print spaces instead of '\t' */
		jumpopt:1,		/* is jumpline defined */
		no_bell:1,		/* suppress bell */
		no_intty:1,		/* is input in interactive mode */
		no_tty:1,		/* is output in interactive mode */
		noscroll_opt:1,		/* do not scroll, clear the screen and then display text */
		notell:1,		/* suppress quit dialog */
		pause:1,		/* is output paused */
		print_names:1,		/* print file name banner */
		rerun_command:1,	/* run previous key command */
		search_called:1,	/* previous more command was a search */
		search_opt:1,		/* is init search pattern defined */
		squeeze_opt:1,		/* suppress white space */
		stdout_glitch:1,	/* terminal has standout mode glitch */
		stop_opt:1,		/* stop after form feeds */
		ul_glitch:1,		/* terminal is underlining in glitch mode */
		ul_opt:1,		/* underline as best we can */
		underlining:1,		/* is underlining going on */
		within:1,		/* true if we are within a file, false if we are between files */
		wrap_margin:1;		/* set if automargins */
};

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
	fputs(_(" -u          suppress underlining\n"), out);
	fputs(_(" -<number>   the number of lines per screenful\n"), out);
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

/* check_magic --
 *	check for file magic numbers.  This code would best be shared
 *	with the file(1) program or, perhaps, more should not try to be
 *	so smart. */
static int check_magic(FILE *f, char *fs)
{
	signed char twobytes[2];

	/* don't try to look ahead if the input is unseekable */
	if (fseek(f, 0L, SEEK_SET))
		return 0;

	if (fread(twobytes, 2, 1, f) == 1) {
		switch (twobytes[0] + (twobytes[1] << 8)) {
		case 0407:	/* a.out obj */
		case 0410:	/* a.out exec */
		case 0413:	/* a.out demand exec */
		case 0405:
		case 0411:
		case 0177545:
		case 0x457f:	/* simple ELF detection */
			printf(_("\n******** %s: Not a text file ********\n\n"),
			       fs);
			return 1;
		}
	}
	fseek(f, 0L, SEEK_SET);	/* rewind() not necessary */
	return 0;
}

static void more_fseek(struct more_control *ctl, FILE *stream, long pos)
{
	ctl->file_pos = pos;
	fseek(stream, pos, 0);
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

	if (stat(fs, &stbuf) == -1) {
		fflush(stdout);
		if (ctl->clreol_opt)
			my_putstring(ctl->eraseln);
		warn(_("stat of %s failed"), fs);
		return NULL;
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFDIR) {
		printf(_("\n*** %s: directory ***\n\n"), fs);
		return NULL;
	}
	if ((f = fopen(fs, "r")) == NULL) {
		fflush(stdout);
		warn(_("cannot open %s"), fs);
		return NULL;
	}
	if (check_magic(f, fs)) {
		fclose(f);
		return NULL;
	}
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
	size_t nsz = ctl->ncolumns * 4;

	if (ctl->linesz >= nsz)
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
	long file_pos_bak = ctl->file_pos;

	memset(&state, 0, sizeof state);
#endif
	prepare_line_buffer(ctl);

	c = more_getc(ctl, f);
	if (colflg && c == '\n') {
		ctl->current_line++;
		c = more_getc(ctl, f);
	}
	while (p < &ctl->linebuf[ctl->linesz - 1]) {
#ifdef HAVE_WIDECHAR
		if (ctl->fold_opt && use_mbc_buffer_flag && MB_CUR_MAX > 1) {
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

				if (column >= ctl->ncolumns)
					more_fseek(ctl, f, file_pos_bak);
				else {
					memmove(mbc, mbc + 1, --mbc_pos);
					if (mbc_pos > 0) {
						mbc[mbc_pos] = '\0';
						goto process_mbc;
					}
				}
				break;

			default:
				wc_width = wcwidth(wc);

				if (column + wc_width > ctl->ncolumns) {
					more_fseek(ctl, f, file_pos_bak);
					break_flag = 1;
				} else {
					for (i = 0; p < &ctl->linebuf[ctl->linesz - 1] &&
						    i < mbc_pos; i++)
						*p++ = mbc[i];
					if (wc_width > 0)
						column += wc_width;
				}
			}

			if (break_flag || column >= ctl->ncolumns)
				break;

			c = more_getc(ctl, f);
			continue;
		}
#endif	/* HAVE_WIDECHAR */
		if (c == EOF) {
			if (p > ctl->linebuf) {
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
					my_putstring(ctl->eraseln);
					ctl->promptlen = 0;
				} else {
					for (--p; p < &ctl->linebuf[ctl->linesz - 1];) {
						*p++ = ' ';
						if ((++column & 7) == 0)
							break;
					}
					if (column >= ctl->promptlen)
						ctl->promptlen = 0;
				}
			} else
				column = 1 + (column | 7);
		} else if (c == '\b' && column > 0) {
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
			ctl->pause = 1;
		} else if (c == EOF) {
			*length = p - ctl->linebuf;
			return column;
		} else {
#ifdef HAVE_WIDECHAR
			if (ctl->fold_opt && MB_CUR_MAX > 1) {
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
					if (wc_width > 0)
						column += wc_width;
				}
			} else
#endif	/* HAVE_WIDECHAR */
			{
				if (isprint(c))
					column++;
			}
		}

		if (column >= ctl->ncolumns && ctl->fold_opt)
			break;
#ifdef HAVE_WIDECHAR
		if (use_mbc_buffer_flag == 0 && p >= &ctl->linebuf[ctl->linesz - 1 - 4])
			/* don't read another char if there is no space for
			 * whole multibyte sequence */
			break;
#endif
		c = more_getc(ctl, f);
	}
	if (column >= ctl->ncolumns && ctl->ncolumns > 0)
		if (!ctl->wrap_margin)
			*p++ = '\n';
	colflg = column == ctl->ncolumns && ctl->fold_opt;
	if (colflg && ctl->eatnl && ctl->wrap_margin)
		*p++ = '\n';	/* simulate normal wrap */
	*length = p - ctl->linebuf;
	*p = 0;
	return column;
}

/* Erase the rest of the prompt, assuming we are starting at column col. */
static void erase_prompt(struct more_control *ctl, int col)
{

	if (ctl->promptlen == 0)
		return;
	if (ctl->hard_term)
		putchar('\n');
	else {
		if (col == 0)
			putchar('\r');
		if (!ctl->dumb && ctl->eraseln)
			my_putstring(ctl->eraseln);
		else
			printf("%*s", ctl->promptlen - col, "");
	}
	ctl->promptlen = 0;
}

static UL_ASAN_BLACKLIST size_t xmbrtowc(wchar_t *wc, const char *s, size_t n,
				  mbstate_t *mbstate)
{
	const size_t mblength = mbrtowc(wc, s, n, mbstate);
	if (mblength == (size_t)-2 || mblength == (size_t)-1)
		return 1;
	return mblength;
}

static int would_underline(char *s, int n)
{
	if (n < 2)
		return 0;
	if ((s[0] == '_' && s[1] == '\b') || (s[1] == '\b' && s[2] == '_'))
		return 1;
	return 0;
}

/* Print a buffer of n characters */
static void print_buffer(struct more_control *ctl, char *s, int n)
{
	char c;			/* next output character */
	int state;		/* next output char's UL state */

	while (--n >= 0) {
		if (!ctl->ul_opt) {
			putchar(*s++);
			continue;
		}
		if (*s == ' ' && ctl->underlining == 0 && ctl->ul_glitch
		    && would_underline(s + 1, n - 1)) {
			s++;
			continue;
		}
		if ((state = would_underline(s, n)) != 0) {
			c = (*s == '_') ? s[2] : *s;
			n -= 2;
			s += 3;
		} else
			c = *s++;
		if (state != ctl->underlining) {
			if (c == ' ' && state == 0 && ctl->ul_glitch
			    && would_underline(s, n - 1))
				state = 1;
			else
				my_putstring(state ? ctl->underline_enter : ctl->
					     underline_exit);
		}
		if (c != ' ' || ctl->underlining == 0 || state != 0
		    || ctl->ul_glitch == 0)
#ifdef HAVE_WIDECHAR
		{
			wchar_t wc;
			size_t mblength;
			mbstate_t mbstate;

			memset(&mbstate, 0, sizeof mbstate);
			s--;
			n++;
			mblength = xmbrtowc(&wc, s, n, &mbstate);
			while (mblength--)
				putchar(*s++);
			n += mblength;
		}
#else
			putchar(c);
#endif				/* HAVE_WIDECHAR */
		if (state && *ctl->underlining_char) {
			fputs(ctl->backspace_char, stdout);
			my_putstring(ctl->underlining_char);
		}
		ctl->underlining = state;
	}
}

/* Erase the current line entirely */
static void kill_line(struct more_control *ctl)
{
	erase_prompt(ctl, 0);
	if (!ctl->eraseln || ctl->dumb)
		putchar('\r');
}

static void output_prompt(struct more_control *ctl, char *filename)
{
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	else if (ctl->promptlen > 0)
		kill_line(ctl);
	if (!ctl->hard_term) {
		ctl->promptlen = 0;
		if (ctl->std_enter && ctl->std_exit) {
			my_putstring(ctl->std_enter);
			ctl->promptlen += (2 * ctl->stdout_glitch);
		}
		if (ctl->clreol_opt)
			my_putstring(ctl->eraseln);
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
		if (ctl->std_enter && ctl->std_exit)
			my_putstring(ctl->std_exit);
		if (ctl->clreol_opt)
			my_putstring(ctl->end_clear);
		fflush(stdout);
	} else
		fputc('\a', stderr);
}

static void reset_tty(struct more_control *ctl)
{
	if (ctl->no_tty)
		return;
	if (ctl->underlining) {
		tputs(ctl->underline_exit, STDOUT_FILENO, putchar);
		fflush(stdout);
		ctl->underlining = 0;
	}
	ctl->otty.c_lflag |= ICANON | ECHO;
	ctl->otty.c_cc[VMIN] = ctl->orig_tty.c_cc[VMIN];
	ctl->otty.c_cc[VTIME] = ctl->orig_tty.c_cc[VTIME];
	tcsetattr(STDERR_FILENO, TCSANOW, &ctl->orig_tty);
}

/* Clean up terminal state and exit. Also come here if interrupt signal received */
static void __attribute__((__noreturn__)) exit_more(struct more_control *ctl)
{
	reset_tty(ctl);
	if (ctl->clreol_opt) {
		putchar('\r');
		my_putstring(ctl->end_clear);
		fflush(stdout);
	} else if (!ctl->clreol_opt && (ctl->promptlen > 0)) {
		kill_line(ctl);
		fflush(stdout);
	} else
		fputc('\n', stderr);
	free(ctl->previousre);
	free(ctl->linebuf);
	_exit(EXIT_SUCCESS);
}

static int read_char(struct more_control *ctl)
{
	unsigned char c;

	errno = 0;
	if (read(STDERR_FILENO, &c, 1) <= 0) {
		if (errno != EINTR)
			exit_more(ctl);
		else
			c = ctl->otty.c_cc[VKILL];
	}
	return c;
}

/* Read a decimal number from the terminal.  Set cmd to the non-digit
 * which terminates the number. */
static int read_number(struct more_control *ctl, char *cmd)
{
	int i = 0;
	char ch;

	for (;;) {
		ch = read_char(ctl);
		if (isdigit(ch))
			i = i * 10 + ch - '0';
		else if ((cc_t)ch == ctl->otty.c_cc[VKILL])
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
	if (nskip > 0) {
		if (ctl->argv_position + nskip > ctl->nfiles - 1)
			nskip = ctl->nfiles - ctl->argv_position - 1;
	} else if (ctl->within)
		ctl->argv_position++;
	ctl->argv_position += nskip;
	if (ctl->argv_position < 0)
		ctl->argv_position = 0;
	puts(_("\n...Skipping "));
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	if (nskip > 0)
		fputs(_("...Skipping to file "), stdout);
	else
		fputs(_("...Skipping back to file "), stdout);
	puts(ctl->fnames[ctl->argv_position]);
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	putchar('\n');
	ctl->argv_position--;
}

static void show(struct more_control *ctl, char c)
{
	if ((c < ' ' && c != '\n' && c != ESC) || c == RUBOUT) {
		c += (c == RUBOUT) ? -0100 : 0100;
		fputs("^", stderr);
		ctl->promptlen++;
	}
	fputc(c, stderr);
	ctl->promptlen++;
}

static void more_error(struct more_control *ctl, char *mess)
{
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	else
		kill_line(ctl);
	ctl->promptlen += strlen(mess);
	if (ctl->std_enter && ctl->std_exit) {
		my_putstring(ctl->std_enter);
		fputs(mess, stdout);
		my_putstring(ctl->std_exit);
	} else
		fputs(mess, stdout);
	fflush(stdout);
	ctl->errors++;
}

static void erase_one_column(struct more_control *ctl)
{
	if (ctl->docrterase)
		fputs("\b \b", stderr);
	else
		fputs("\b", stderr);
}

static void ttyin(struct more_control *ctl, char buf[], int nmax, char pchar)
{
	char *sp = buf;
	int c;
	int slash = 0;
	int maxlen = 0;

	while (sp - buf < nmax) {
		if (ctl->promptlen > maxlen)
			maxlen = ctl->promptlen;
		c = read_char(ctl);
		if (c == '\\')
			slash++;
		else if (((cc_t) c == ctl->otty.c_cc[VERASE]) && !slash) {
			if (sp > buf) {
#ifdef HAVE_WIDECHAR
				if (MB_CUR_MAX > 1) {
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

						if (buf + pos + mblength >= sp)
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

				if ((*sp < ' ' && *sp != '\n') || *sp == RUBOUT) {
					ctl->promptlen--;
					erase_one_column(ctl);
				}
				continue;
			} else {
				if (!ctl->eraseln)
					ctl->promptlen = maxlen;
			}
		} else if (((cc_t) c == ctl->otty.c_cc[VKILL]) && !slash) {
			if (ctl->hard_term) {
				show(ctl, c);
				putchar('\n');
				putchar(pchar);
			} else {
				putchar('\r');
				putchar(pchar);
				if (ctl->eraseln)
					erase_prompt(ctl, 1);
				else if (ctl->docrtkill)
					while (ctl->promptlen-- > 1)
						fputs("\b \b", stderr);
				ctl->promptlen = 1;
			}
			sp = buf;
			fflush(stdout);
			continue;
		}
		if (slash && ((cc_t) c == ctl->otty.c_cc[VKILL]
			      || (cc_t) c == ctl->otty.c_cc[VERASE])) {
			erase_one_column(ctl);
			sp--;
		}
		if (c != '\\')
			slash = 0;
		*sp++ = c;
		if ((c < ' ' && c != '\n' && c != ESC) || c == RUBOUT) {
			c += (c == RUBOUT) ? -0100 : 0100;
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
	if (sp - buf >= nmax - 1)
		more_error(ctl, _("Line too long"));
}

static int command_expansion(struct more_control *ctl, char **outbuf, char *inbuf)
{
	char *inpstr = inbuf;
	char *outstr;
	char c;
	char *temp;
	int changed = 0;
	int tempsz, xtra, offset;

	xtra = strlen(ctl->fnames[ctl->argv_position]) + strlen(ctl->shell_line) + 1;
	tempsz = 200 + xtra;
	temp = xmalloc(tempsz);
	outstr = temp;
	while ((c = *inpstr++) != '\0') {
		offset = outstr - temp;
		if (tempsz - offset - 1 < xtra) {
			tempsz += 200 + xtra;
			temp = xrealloc(temp, tempsz);
			outstr = temp + offset;
		}
		switch (c) {
		case '%':
			if (!ctl->no_intty) {
				strcpy(outstr, ctl->fnames[ctl->argv_position]);
				outstr += strlen(ctl->fnames[ctl->argv_position]);
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
	*outbuf = temp;
	return changed;
}

static void set_tty(struct more_control *ctl)
{
	ctl->otty.c_lflag &= ~(ICANON | ECHO);
	ctl->otty.c_cc[VMIN] = 1;	/* read at least 1 char */
	ctl->otty.c_cc[VTIME] = 0;	/* no timeout */
	tcsetattr(STDERR_FILENO, TCSANOW, &ctl->otty);
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
	fflush(stdout);
	kill(0, SIGSTOP);
	/* We're back */
	set_tty(ctl);
}

static void execute(struct more_control *ctl, char *filename, char *cmd, ...)
{
	int id;
	int n;
	va_list argp;
	char *arg;
	char **args;
	int argcount;

	fflush(stdout);
	reset_tty(ctl);
	for (n = 10; (id = fork()) < 0 && n > 0; n--)
		sleep(5);
	if (id == 0) {
		if (!isatty(STDIN_FILENO)) {
			close(STDIN_FILENO);
			if (open("/dev/tty", 0) < 0) {
				fprintf(stderr, _("cannot open %s"), "/dev/tty\n");
				goto err;
			}
		}

		va_start(argp, cmd);
		arg = va_arg(argp, char *);
		argcount = 0;
		while (arg) {
			argcount++;
			arg = va_arg(argp, char *);
		}
		va_end(argp);

		args = alloca(sizeof(char *) * (argcount + 1));
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

		execvp(cmd, args);
		fputs(_("exec failed\n"), stderr);
		exit(EXIT_FAILURE);
	}
	if (id > 0) {
		while (wait(0) > 0)
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

	kill_line(ctl);
	putchar('!');
	fflush(stdout);
	ctl->promptlen = 1;
	if (ctl->rerun_command)
		fputs(ctl->shell_line, stdout);
	else {
		ttyin(ctl, cmdbuf, sizeof(cmdbuf) - 2, '!');
		rc = command_expansion(ctl, &expanded, cmdbuf);
		if (expanded) {
			if (strlen(expanded) < sizeof(ctl->shell_line))
				strcpy(ctl->shell_line, expanded);
			else
				rc = -1;
			free(expanded);
		}
		if (rc < 0) {
			fputs(_("  Overflow\n"), stderr);
			output_prompt(ctl, filename);
			return;
		} else if (rc > 0) {
			kill_line(ctl);
			ctl->promptlen = printf("!%s", ctl->shell_line);
		}
	}
	fflush(stdout);
	fputc('\n', stderr);
	ctl->promptlen = 0;
	ctl->shellp = 1;
	execute(ctl, filename, ctl->shell, ctl->shell, "-c", ctl->shell_line, 0);
}

/* Execute a colon-prefixed command.  Returns <0 if not a command that
 * should cause more of the file to be printed. */
static int colon_command(struct more_control *ctl, char *filename, int cmd, int nlines)
{
	char ch;

	if (cmd == 0)
		ch = read_char(ctl);
	else
		ch = cmd;
	ctl->lastcolon = ch;
	switch (ch) {
	case 'f':
		kill_line(ctl);
		if (!ctl->no_intty)
			ctl->promptlen =
			    printf(_("\"%s\" line %d"), ctl->fnames[ctl->argv_position], ctl->current_line);
		else
			ctl->promptlen = printf(_("[Not a file] line %d"), ctl->current_line);
		fflush(stdout);
		return -1;
	case 'n':
		if (nlines == 0) {
			if (ctl->argv_position >= ctl->nfiles - 1)
				exit_more(ctl);
			nlines++;
		}
		putchar('\r');
		erase_prompt(ctl, 0);
		change_file(ctl, nlines);
		return 0;
	case 'p':
		if (ctl->no_intty) {
			fputc('\a', stderr);
			return -1;
		}
		putchar('\r');
		erase_prompt(ctl, 0);
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
	int c, n = ctl->jumpline;

	while (n > 0) {
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
		my_putstring(ctl->clear);
		/* Put out carriage return so that system doesn't get
		 * confused by escape sequences when expanding tabs */
		putchar('\r');
		ctl->promptlen = 0;
	}
}

static void read_line(struct more_control *ctl, FILE *f)
{
	int c;
	char *p;

	prepare_line_buffer(ctl);

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
	erase_prompt(ctl, 0);
	alarm(SEARCH_TIMEOUT);
	alarm_received = 0;
	return rpmatch(buf);
}

/* Search for nth occurrence of regular expression contained in buf in
 * the file */
static void search(struct more_control *ctl, char buf[], FILE *file, int n)
{
	long startline = ctl->file_pos;
	long line1 = startline;
	long line2 = startline;
	long line3;
	int lncount = 0;
	int saveln, rc;
	regex_t re;

	ctl->context.line = saveln = ctl->current_line;
	ctl->context.nrow = startline;
	if (!buf)
		goto notfound;
	if ((rc = regcomp(&re, buf, REG_NOSUB)) != 0) {
		char s[REGERR_BUF];
		regerror(rc, &re, s, sizeof s);
		more_error(ctl, s);
		return;
	}
	alarm_received = 0;
	signal(SIGALRM, sig_alarm_handler);
	alarm(SEARCH_TIMEOUT);
	while (!feof(file)) {
		if (alarm_received && stop_search(ctl))
			break;
		line3 = line2;
		line2 = line1;
		line1 = ctl->file_pos;
		read_line(ctl, file);
		lncount++;
		if (regexec(&re, ctl->linebuf, 0, NULL, 0) == 0) {
			if (--n == 0) {
				if (lncount > 3 || (lncount > 1 && ctl->no_intty)) {
					putchar('\n');
					if (ctl->clreol_opt)
						my_putstring(ctl->eraseln);
					fputs(_("...skipping\n"), stdout);
				}
				if (!ctl->no_intty) {
					ctl->current_line -=
					    (lncount >= 3 ? 3 : lncount);
					more_fseek(ctl, file, line3);
					if (ctl->noscroll_opt) {
						if (ctl->clreol_opt) {
							my_putstring(ctl->go_home);
							my_putstring(ctl->eraseln);
						} else
							clear_tty(ctl);
					}
				} else {
					kill_line(ctl);
					if (ctl->noscroll_opt) {
						if (ctl->clreol_opt) {
							my_putstring(ctl->go_home);
							my_putstring(ctl->eraseln);
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
	fputs(  "\n", stdout);
	fputs(_("Most commands optionally preceded by integer argument k.\n"), stdout);
	fputs(_("Defaults in brackets.\n"), stdout);
	fputs(_("Star (*) indicates argument becomes new default.\n"), stdout);
	fputs(  "-------------------------------------------------------------------------------\n", stdout);
	fputs(_("<space>                 Display next k lines of text [current screen size]\n"), stdout);
	fputs(_("z                       Display next k lines of text [current screen size]*\n"), stdout);
	fputs(_("<return>                Display next k lines of text [1]*\n"), stdout);
	fputs(_("d or ctrl-D             Scroll k lines [current scroll size, initially 11]*\n"), stdout);
	fputs(_("q or Q or <interrupt>   Exit from more\n"), stdout);
	fputs(_("s                       Skip forward k lines of text [1]\n"), stdout);
	fputs(_("f                       Skip forward k screenfuls of text [1]\n"), stdout);
	fputs(_("b or ctrl-B             Skip backwards k screenfuls of text [1]\n"), stdout);
	fputs(_("'                       Go to place where previous search started\n"), stdout);
	fputs(_("=                       Display current line number\n"), stdout);
	fputs(_("/<regular expression>   Search for kth occurrence of regular expression [1]\n"), stdout);
	fputs(_("n                       Search for kth occurrence of last r.e [1]\n"), stdout);
	fputs(_("!<cmd> or :!<cmd>       Execute <cmd> in a subshell\n"), stdout);	fprintf(stdout, _(
		"v                       Start up '%s' at current line\n"), find_editor());
	fputs(_("ctrl-L                  Redraw screen\n"), stdout);
	fputs(_(":n                      Go to kth next file [1]\n"), stdout);
	fputs(_(":p                      Go to kth previous file [1]\n"), stdout);
	fputs(_(":f                      Display current file name and line number\n"), stdout);
	fputs(_(".                       Repeat previous command\n"), stdout);
	fputs(  "-------------------------------------------------------------------------------\n", stdout);
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
	kill_line(ctl);
	printf("%s %s %s", editor, cmdbuf, ctl->fnames[ctl->argv_position]);
	if (split) {
		cmdbuf[2] = 0;
		execute(ctl, filename, editor, editor,
			cmdbuf, cmdbuf + 3, ctl->fnames[ctl->argv_position], (char *)0);
	} else
		execute(ctl, filename, editor, editor, cmdbuf, ctl->fnames[ctl->argv_position], (char *)0);
}

static int skip_backwards(struct more_control *ctl, FILE *f, int nlines)
{
	if (nlines == 0)
		nlines++;
	putchar('\r');
	erase_prompt(ctl, 0);
	putchar('\n');
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	printf(P_("...back %d page", "...back %d pages", nlines), nlines);
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	putchar('\n');
	ctl->jumpline = ctl->current_line - ctl->lines_per_screen * (nlines + 1);
	if (!ctl->noscroll_opt)
		ctl->jumpline--;
	if (ctl->jumpline < 0)
		ctl->jumpline = 0;
	more_fseek(ctl, f, 0);
	ctl->current_line = 0;	/* skip_lines() will make current_line correct */
	skip_lines(ctl, f);
	if (!ctl->noscroll_opt)
		return ctl->lines_per_screen + 1;
	return ctl->lines_per_screen;
}

static int skip_forwards(struct more_control *ctl, FILE *f, int nlines, char comchar)
{
	if (nlines == 0)
		nlines++;
	if (comchar == 'f')
		nlines *= ctl->lines_per_screen;
	putchar('\r');
	erase_prompt(ctl, 0);
	putchar('\n');
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	printf(P_("...skipping %d line", "...skipping %d lines", nlines), nlines);
	if (ctl->clreol_opt)
		my_putstring(ctl->eraseln);
	putchar('\n');
	while (nlines > 0) {
		int c;
		while ((c = more_getc(ctl, f)) != '\n')
			if (c == EOF)
				return 0;
		ctl->current_line++;
		nlines--;
	}
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
			ctl->nscroll = ctl->lines_per_page / 2 - 1;
			if (ctl->nscroll <= 0)
				ctl->nscroll = 1;
			ctl->lines_per_screen = ctl->lines_per_page - 1;
		}
		if (win.ws_col != 0)
			ctl->ncolumns = win.ws_col;
	}
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
		if ((cc_t) comchar == ctl->otty.c_cc[VERASE]) {
			kill_line(ctl);
			output_prompt(ctl, filename);
			continue;
		}
		switch (comchar) {
		case ':':
			retval = colon_command(ctl, filename, colonch, nlines);
			if (retval >= 0)
				done++;
			break;
		case 'b':
		case ctrl('B'):
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
		case ctrl('D'):
			if (nlines != 0)
				ctl->nscroll = nlines;
			retval = ctl->nscroll;
			done = 1;
			break;
		case 'q':
		case 'Q':
			exit_more(ctl);
		case 's':
		case 'f':
		case ctrl('F'):
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
				more_fseek(ctl, f, ctl->screen_start.nrow);
				ctl->current_line = ctl->screen_start.line;
				retval = ctl->lines_per_screen;
				done = 1;
				break;
			}
			fputc('\a', stderr);
			break;
		case '\'':
			if (!ctl->no_intty) {
				kill_line(ctl);
				fputs(_("\n***Back***\n\n"), stdout);
				more_fseek(ctl, f, ctl->context.nrow);
				ctl->current_line = ctl->context.line;
				retval = lines;
				done = 1;
				break;
			}
			fputc('\a', stderr);
			break;
		case '=':
			kill_line(ctl);
			ctl->promptlen = printf("%d", ctl->current_line);
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
			kill_line(ctl);
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
				kill_line(ctl);
				if (ctl->std_enter && ctl->std_exit) {
					my_putstring(ctl->std_enter);
					ctl->promptlen =
					    printf(_
						   ("[Press 'h' for instructions.]"))
					    + 2 * ctl->stdout_glitch;
					my_putstring(ctl->std_exit);
				} else
					ctl->promptlen =
					    printf(_
						   ("[Press 'h' for instructions.]"));
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
		while (num_lines > 0 && !ctl->pause) {
			if ((nchars = get_line(ctl, f, &length)) == EOF) {
				if (ctl->clreol_opt)
					my_putstring(ctl->end_clear);
				return;
			}
			if (ctl->squeeze_opt && length == 0 && prev_len == 0)
				continue;
			prev_len = length;
			if (ctl->bad_so
			    || ((ctl->std_enter && *ctl->std_enter == ' ') && (ctl->promptlen > 0)))
				erase_prompt(ctl, 0);
			/* must clear before drawing line since tabs on
			 * some terminals do not erase what they tab
			 * over. */
			if (ctl->clreol_opt)
				my_putstring(ctl->eraseln);
			print_buffer(ctl, ctl->linebuf, length);
			if (nchars < ctl->promptlen)
				erase_prompt(ctl, nchars);	/* erase_prompt () sets promptlen to 0 */
			else
				ctl->promptlen = 0;
			if (nchars < ctl->ncolumns || !ctl->fold_opt)
				print_buffer(ctl, "\n", 1);	/* will turn off UL if necessary */
			if (nchars == STOP)
				break;
			num_lines--;
		}
		if (ctl->underlining) {
			my_putstring(ctl->underline_exit);
			ctl->underlining = 0;
		}
		fflush(stdout);
		if ((c = more_getc(ctl, f)) == EOF) {
			if (ctl->clreol_opt)
				my_putstring(ctl->end_clear);
			return;
		}

		if (ctl->pause && ctl->clreol_opt)
			my_putstring(ctl->end_clear);
		more_ungetc(ctl, c, f);
		ctl->pause = 0;
		do {
			if ((num_lines = command(ctl, NULL, f)) == 0)
				return;
		} while (ctl->search_called && !ctl->previousre);
		if (ctl->hard_term && ctl->promptlen > 0)
			erase_prompt(ctl, 0);
		if (ctl->noscroll_opt && num_lines >= ctl->lines_per_screen) {
			if (ctl->clreol_opt)
				my_putstring(ctl->go_home);
			else
				clear_tty(ctl);
		}
		ctl->screen_start.line = ctl->current_line;
		ctl->screen_start.nrow = ctl->file_pos;
	}
}

static void copy_file(FILE *f)
{
	char buf[BUFSIZ];
	size_t sz;

	while ((sz = fread(&buf, sizeof(char), sizeof(buf), f)) > 0)
		fwrite(&buf, sizeof(char), sz, stdout);
}

static void initterm(struct more_control *ctl)
{
	int ret, tmp;
	char *term;
#ifdef TIOCGWINSZ
	struct winsize win;
#endif

#ifndef NON_INTERACTIVE_MORE
	ctl->no_tty = tcgetattr(STDOUT_FILENO, &ctl->otty);
#endif
	if (!ctl->no_tty) {
		ctl->docrterase = (ctl->otty.c_cc[VERASE] != 255);
		ctl->docrtkill = (ctl->otty.c_cc[VKILL] != 255);
		if ((term = getenv("TERM")) == NULL) {
			ctl->dumb = 1;
			ctl->ul_opt = 0;
		}
		my_setupterm(term, 1, &ret);
		if (ret <= 0) {
			ctl->dumb = 1;
			ctl->ul_opt = 0;
		} else {
#ifdef TIOCGWINSZ
			if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &win) < 0) {
#endif
				ctl->lines_per_page = my_tgetnum(TERM_LINES);
				ctl->ncolumns = my_tgetnum(TERM_COLS);
#ifdef TIOCGWINSZ
			} else {
				if ((ctl->lines_per_page = win.ws_row) == 0)
					ctl->lines_per_page = my_tgetnum(TERM_LINES);
				if ((ctl->ncolumns = win.ws_col) == 0)
					ctl->ncolumns = my_tgetnum(TERM_COLS);
			}
#endif
			if ((ctl->lines_per_page <= 0) || my_tgetflag(TERM_HARD_COPY)) {
				ctl->hard_term = 1;
				ctl->lines_per_page = LINES_PER_PAGE;
			}

			if (my_tgetflag(TERM_EAT_NEW_LINE))
				/* Eat newline at last column + 1; dec, concept */
				ctl->eatnl = 1;
			if (ctl->ncolumns <= 0)
				ctl->ncolumns = NUM_COLUMNS;

			ctl->wrap_margin = my_tgetflag(TERM_AUTO_RIGHT_MARGIN);
			ctl->bad_so = my_tgetflag(TERM_CEOL);
			ctl->eraseln = my_tgetstr(TERM_CLEAR_TO_LINE_END);
			ctl->clear = my_tgetstr(TERM_CLEAR);
			ctl->std_enter = my_tgetstr(TERM_STANDARD_MODE);
			ctl->std_exit = my_tgetstr(TERM_EXIT_STANDARD_MODE);
			tmp = my_tgetnum(TERM_STD_MODE_GLITCH);
			if (tmp < 0)
				ctl->stdout_glitch = 0;
			else if (0 < tmp)
				ctl->stdout_glitch = 1;

			/* Set up for underlining:  some terminals don't
			 * need it; others have start/stop sequences,
			 * still others have an underline char sequence
			 * which is assumed to move the cursor forward
			 * one character.  If underline sequence isn't
			 * available, settle for standout sequence. */
			if (my_tgetflag(TERM_UNDERLINE)
			    || my_tgetflag(TERM_OVER_STRIKE))
				ctl->ul_opt = 0;
			if ((ctl->underlining_char = my_tgetstr(TERM_UNDERLINE_CHAR)) == NULL)
				ctl->underlining_char = "";
			if (((ctl->underline_enter =
			      my_tgetstr(TERM_ENTER_UNDERLINE)) == NULL
			     || (ctl->underline_exit =
				 my_tgetstr(TERM_EXIT_UNDERLINE)) == NULL)
			    && !*ctl->underlining_char) {
				if ((ctl->underline_enter = ctl->std_enter) == NULL
				    || (ctl->underline_exit = ctl->std_exit) == NULL) {
					ctl->underline_enter = "";
					ctl->underline_exit = "";
				} else
					ctl->ul_glitch = ctl->stdout_glitch;
			} else {
				ctl->ul_glitch = 0;
			}

			ctl->go_home = my_tgetstr(TERM_HOME);
			if (ctl->go_home == NULL || *ctl->go_home == '\0') {
				if ((ctl->cursorm =
				     my_tgetstr(TERM_CURSOR_ADDRESS)) != NULL) {
					const char *t =
					    (const char *)my_tgoto(ctl->cursorm, 0,
								   0);
					xstrncpy(ctl->cursorhome, t,
						 sizeof(ctl->cursorhome));
					ctl->go_home = ctl->cursorhome;
				}
			}
			ctl->end_clear = my_tgetstr(TERM_CLEAR_TO_SCREEN_END);
			if ((ctl->backspace_char = my_tgetstr(TERM_LINE_DOWN)) == NULL)
				ctl->backspace_char = "\b";

		}
		if ((ctl->shell = getenv("SHELL")) == NULL)
			ctl->shell = _PATH_BSHELL;
	}
	ctl->no_intty = tcgetattr(STDIN_FILENO, &ctl->otty);
	tcgetattr(STDERR_FILENO, &ctl->otty);
	ctl->orig_tty = ctl->otty;
	ctl->hardtabs = (ctl->otty.c_oflag & TABDLY) != XTABS;
	if (!ctl->no_tty) {
		ctl->otty.c_lflag &= ~(ICANON | ECHO);
		ctl->otty.c_cc[VMIN] = 1;
		ctl->otty.c_cc[VTIME] = 0;
	}
}

static void display_file(struct more_control *ctl, FILE *f, char *initbuf, int left)
{
	ctl->context.line = ctl->context.nrow = 0;
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
		} else if (ctl->jumpopt)
			skip_lines(ctl, f);
	} else if (ctl->argv_position < ctl->nfiles && !ctl->no_tty)
		left = command(ctl, ctl->fnames[ctl->argv_position], f);
	if (left != 0) {
		if ((ctl->noscroll_opt || ctl->clearfirst)
		    && (ctl->file_size != LONG_MAX)) {
			if (ctl->clreol_opt)
				my_putstring(ctl->go_home);
			else
				clear_tty(ctl);
		}
		if (ctl->print_names) {
			if (ctl->bad_so)
				erase_prompt(ctl, 0);
			if (ctl->clreol_opt)
				my_putstring(ctl->eraseln);
			fputs("::::::::::::::", stdout);
			if (ctl->promptlen > 14)
				erase_prompt(ctl, 14);
			putchar('\n');
			if (ctl->clreol_opt)
				my_putstring(ctl->eraseln);
			puts(ctl->fnames[ctl->argv_position]);
			if (ctl->clreol_opt)
				my_putstring(ctl->eraseln);
			puts("::::::::::::::");
			if (left > ctl->lines_per_page - 4)
				left = ctl->lines_per_page - 4;
		}
		if (ctl->no_tty)
			copy_file(f);
		else {
			ctl->within = 1;
			screen(ctl, f, left);
			ctl->within = 0;
		}
	}
	fflush(stdout);
	fclose(f);
	ctl->screen_start.line = ctl->screen_start.nrow = 0L;
	ctl->context.line = ctl->context.nrow = 0L;
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
		.ncolumns = NUM_COLUMNS,
		.nscroll = SCROLL_LEN,
		0
	};

	setlocale(LC_ALL, "");
	bindtextdomain(PACKAGE, LOCALEDIR);
	textdomain(PACKAGE);
	atexit(close_stdout);

	ctl.nfiles = argc;
	ctl.fnames = argv;
	setlocale(LC_ALL, "");
	initterm(&ctl);

	/* Auto set no scroll on when binary is called page */
	if (!(strcmp(program_invocation_short_name, "page")))
		ctl.noscroll_opt = 1;

	prepare_line_buffer(&ctl);

	ctl.nscroll = ctl.lines_per_page / 2 - 1;
	if (ctl.nscroll <= 0)
		ctl.nscroll = 1;

	if ((s = getenv("MORE")) != NULL)
		argscan(&ctl, s);

	while (--ctl.nfiles > 0) {
		if ((c = (*++ctl.fnames)[0]) == '-') {
			argscan(&ctl, *ctl.fnames + 1);
		} else if (c == '+') {
			s = *ctl.fnames;
			if (*++s == '/') {
				ctl.search_opt = 1;
				initbuf = xstrdup(s + 1);
			} else {
				ctl.jumpopt = 1;
				for (ctl.jumpline = 0; *s != '\0'; s++)
					if (isdigit(*s))
						ctl.jumpline =
						    ctl.jumpline * 10 + *s - '0';
				ctl.jumpline--;
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
	if (ctl.nfiles > 1)
		ctl.print_names = 1;
	if (!ctl.no_intty && ctl.nfiles == 0)
		usage(stderr);
	if (!ctl.no_tty)
		tcsetattr(STDERR_FILENO, TCSANOW, &ctl.otty);
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

	for (/* nothing */; ctl.argv_position < ctl.nfiles; ctl.argv_position++) {
		if ((f = more_fopen(&ctl, ctl.fnames[ctl.argv_position])) == NULL)
			continue;
		display_file(&ctl, f, initbuf, left);
		ctl.first_file = 0;
	}
	free(ctl.previousre);
	free(initbuf);
	free(ctl.linebuf);
	reset_tty(&ctl);
	return EXIT_SUCCESS;
}
