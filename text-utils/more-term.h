#if defined(HAVE_NCURSES_H) || defined(HAVE_NCURSES_NCURSES_H)

# include <term.h>
# define TERM_AUTO_RIGHT_MARGIN		"am"
# define TERM_CEOL			"xhp"
# define TERM_CLEAR			"clear"
# define TERM_CLEAR_TO_LINE_END		"el"
# define TERM_CLEAR_TO_SCREEN_END	"ed"
# define TERM_COLS			"cols"
# define TERM_CURSOR_ADDRESS		"cup"
# define TERM_EAT_NEW_LINE		"xenl"
# define TERM_ENTER_UNDERLINE		"smul"
# define TERM_EXIT_STANDARD_MODE	"rmso"
# define TERM_EXIT_UNDERLINE		"rmul"
# define TERM_HARD_COPY			"hc"
# define TERM_HOME			"home"
# define TERM_LINES			"lines"
# define TERM_LINE_DOWN			"cud1"
# define TERM_OVER_STRIKE		"os"
# define TERM_STANDARD_MODE		"smso"
# define TERM_STD_MODE_GLITCH		"xmc"
# define TERM_UNDERLINE			"ul"
# define TERM_UNDERLINE_CHAR		"uc"

static inline void my_putstring(char *s)
{
	tputs(s, fileno(stdout), putchar);	/* putp(s); */
}

static inline void my_setupterm(char *term, int fildes, int *errret)
{
	setupterm(term, fildes, errret);
}

static inline int my_tgetnum(char *s)
{
	return tigetnum(s);
}

static inline int my_tgetflag(char *s)
{
	return tigetflag(s);
}

static inline char *my_tgetstr(char *s)
{
	return tigetstr(s);
}

static inline char *my_tgoto(char *cap, int col, int row)
{
	return tparm(cap, col, row);
}

#elif defined(HAVE_LIBTERMCAP)	/* ncurses not found */

# include <termcap.h>
# define TERM_AUTO_RIGHT_MARGIN		"am"
# define TERM_CEOL			"xs"
# define TERM_CLEAR			"cl"
# define TERM_CLEAR_TO_LINE_END		"ce"
# define TERM_CLEAR_TO_SCREEN_END	"cd"
# define TERM_COLS			"co"
# define TERM_CURSOR_ADDRESS		"cm"
# define TERM_EAT_NEW_LINE		"xn"
# define TERM_ENTER_UNDERLINE		"us"
# define TERM_EXIT_STANDARD_MODE	"se"
# define TERM_EXIT_UNDERLINE		"ue"
# define TERM_HARD_COPY			"hc"
# define TERM_HOME			"ho"
# define TERM_LINES			"li"
# define TERM_LINE_DOWN			"le"
# define TERM_OVER_STRIKE		"os"
# define TERM_STANDARD_MODE		"so"
# define TERM_STD_MODE_GLITCH		"sg"
# define TERM_UNDERLINE			"ul"
# define TERM_UNDERLINE_CHAR		"uc"

static inline void my_putstring(char *s)
{
	tputs(s, fileno(stdout), putchar);
}

static inline void my_setupterm(char *term, int fildes
				__attribute__ ((__unused__)), int *errret)
{
	*errret = tgetent(tcbuffer, term);
}

static inline int my_tgetnum(char *s)
{
	return tgetnum(s);
}

static inline int my_tgetflag(char *s)
{
	return tgetflag(s);
}

static inline char *my_tgetstr(char *s)
{
	return tgetstr(s, &strbuf);
}

static inline char *my_tgoto(char *cap, int col, int row)
{
	return tgoto(cap, col, row);
}

#endif				/* HAVE_LIBTERMCAP */
