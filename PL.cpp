#include "ptoc.h"
#include<stdlib.h>


#ifndef WIN32
void _splitpath(const char *path, char *drive, char *dir, char *fname, char *ext);
void _makepath(char *path,const char *drive, char *dir, char *fname, char *ext);
static void _split_whole_name(const char *whole_name, char *fname, char *ext);
#endif


/*******************************
*Constants
*******************************/
const int norw = 28;       /* no. of reserved words */
const int txmax = 100;     /* length of identifier table */
const int bmax = 20;       /* length of block inormation table */
const int arrmax = 30;     /* length of array information table */
const int nmax = 6;        /* max. no. of digits in numbers */
const int al = 10;         /* length of identifiers */
const int amax = 2047;     /* maxinum address */
const int levmax = 7;      /* maxinum depth of block nesting */
const int cxmax = 1000;    /* size of code array */


const int min_index1 = -32767;
const int max_index1 = +32767;

/*******************************/

/*******************************
*Enums
*******************************/
enum symbol {nul, ident, intcon, charcon, plus, minus, times, divsym,
             eql, neq, lss, leq, gtr, geq, ofsym, arraysym, programsym, modsym,
             andsym, orsym, notsym, lbrack, rbrack, lparen, rparen, comma,
             semicolon, period, becomes, colon, beginsym, endsym, ifsym, thensym,
             elsesym, whilesym, repeatsym, dosym,callsym, constsym, typesym,
             varsym, procsym, forsym,casesym,untilsym,tosym,downtosym,forwardsym,functionsym,integersym,last_symbol
            };
enum oobject {konstant, typel, variable, prosedure,function, last_oobject};
enum types {notyp, ints, chars, bool_, arrays, last_types};
enum opcod {lit, lod, ilod, loda, lodt, sto, lodb, cpyb, jmp, jpc, red, wrt,
            cal, retp, endp, udis, opac, entp, ands, ors, nots, imod, mus, add,
            sub, mult, idiv, eq, ne, ls, le, gt, ge,opfc,last_opcod
           };  /* opration code */
/*******************************/

/*******************************
*Structures
*******************************/

typedef varying_string<al> alfa;/*String with length al*/
typedef short index1;
typedef set_of_enum(symbol) symset;

struct instruction
{
    opcod f;
    unsigned char l;
    unsigned short a;
};

struct item
{
    types typ;
    int ref;
};

struct A1
{
    alfa name;
    oobject kind;
    types typ;
    unsigned char lev;
    boolean normal;
    index1 ref;
    index1 link;

    union
    {
        int adr;
        int val;
        int size;
    };
};//Elements of nametab

struct A2
{
    types inxtyp, eltyp;
    index1 elref, low, high, elsize, size;
};//Elements of atab

struct B1
{
    index1 last, lastpar, psize, vsize;
};//Elements of btab

struct constrec
{
    types tp;
    int i;
};


/*******************************
*Global variables
*******************************/
char ch;           /* last character read */
symbol sym;        /* last symbol read*/
alfa id;           /* last identifier read*/
int num;       /* last number read */
int cc;        /* character count */
int ll;        /* line length */
int kk, err;
varying_string<81> line;
alfa a;
int i;
array<1, norw, alfa> word1;
array<1, norw, symbol> wsym;
array < -128, 127, symbol > ssym;
array<0, last_opcod, varying_string<5> > mnemonic;

symset declbegsys, statbegsys, facbegsys, constbegsys, typebegsys;

array<0, txmax, A1> nametab;/* name table */
int tx;             /* index of nametab*/

array<1, amax, A2> atab;/* array information table */
int ax;             /*index of atab */

array<0, bmax, B1> btab;/* block information table */
int bx;                 /* index of btab */

array<0, levmax, int> display;

array<0, cxmax, instruction> code;
int cx;          /* code allocation index */

array<0, 100, int> labtab;
int lx;

text sfile;       /* source program file */
string sfile_buff;
char sfilename[_MAX_PATH]; /* source program file name */

file<instruction> fcode;
file<int> labfile;

text listfile;
string listfile_buff;
char listfilename[_MAX_PATH];

char dir[_MAX_DIR];
char name[_MAX_FNAME];
char ext[_MAX_EXT];

/*********************************************************/

/*******************************
*Function Decalarations
*******************************/
void initial();
void enterpreid();
void enter(alfa x0, oobject x1, types x2, int x3);
void error(int n);
void getch();
void getsym();
void enterarray (types tp , int l, int h);
void enterblock();
void gen(opcod x, int y, int z);
void test(symset s1, symset s2, int n);
void block( symset fsys, int level);
void enter1( oobject k, int &level);
int position(alfa id, int &level);
void constant(symset fsys, constrec &c, int &level);
void typ(symset fsys, types &tp, int &rf, int &sz, int &level);
void arraytyp(int &aref, int &arsz, int &level, symset &fsys);
void typ(symset fsys, types &tp, int &rf, int &sz, int &level);
void paramenterlist(symset &fsys, int &level, int &dx);
void constdeclaration(int &level, symset &fsys);
void typedeclaration(int &level, symset &fsys);
void vardeclaration(int &level, symset &fsys, int &dx);
void procdeclaration(int &level, symset &fsys);
void listcode(int &cx0);
void statement(symset fsys, int &level);
void expression(symset fsys, item &x, int &level);
void simpleexpression(symset fsys, item &x, int &level);
void term(symset fsys, item &x, int &level);
void arrayelement(symset fsys, item &x, int &level);
void factor(symset fsys, item &x, int &level);
void assignment(int &i, int &level, symset &fsys);
void ifstatement(int &level, symset &fsys, int &cx1, int &cx2);
void compound(symset &fsys, int &level);
void whilestatement(int &cx1, int &level, symset &fsys, int &cx2);
void stanproc(int i, int &level, item &x, symset &fsys);
void call(int &level, symset &fsys);
void fsplit(string sfilename, string dir, string name, string ext);
void forstatement(int &cx1, int &level, symset &fsys, int &cx2);
void repeatstatement(int &cx1, int &level, symset &fsys, int &cx2);
void casestatement(int &cx1, int &level, symset &fsys, int &cx2);
void functioncall(int &level,symset &fsys);

/*******************************/
void initial()
{
    word1[ 1] = "and       ";
    word1[ 2] = "array     ";
    word1[ 3] = "begin     ";
    word1[ 4] = "call      ";
	word1[ 5] = "case      ";
    word1[ 6] = "const     ";
    word1[ 7] = "do        ";
	word1[ 8] = "downto    ";
    word1[ 9] = "else      ";
    word1[10] = "end       ";
	word1[11] = "for       ";
	word1[12] = "forward   ";
	word1[13] = "function  ";
    word1[14] = "if        ";
	//word1[15] = "integer   ";
    word1[15] = "mod       ";
    word1[16] = "not       ";
    word1[17] = "of        ";
    word1[18] = "or        ";
    word1[19] = "procedure ";
    word1[20] = "program   ";
    word1[21] = "repeat    ";
    word1[22] = "then      ";
	word1[23] = "to        ";
	word1[24] = "type      ";
	word1[25] = "until     ";
    word1[26] = "var       ";
    word1[27] = "while     ";

    wsym[ 1] = andsym;
    wsym[ 2] = arraysym;
    wsym[ 3] = beginsym;
    wsym[ 4] = callsym;
	wsym[ 5] = casesym;
    wsym[ 6] = constsym;
    wsym[ 7] = dosym;
	wsym[ 8] = downtosym;
    wsym[ 9] = elsesym;
    wsym[10] = endsym;
	wsym[11] = forsym;
	wsym[12] = forwardsym;
	wsym[13] = functionsym;
    wsym[14] = ifsym;
	//wsym[15] = integersym;
    wsym[15] = modsym;
    wsym[16] = notsym;
    wsym[17] = ofsym;
    wsym[18] = orsym;
    wsym[19] = procsym;
    wsym[20] = programsym;
    wsym[21] = repeatsym;
    wsym[22] = thensym;
	wsym[23] = tosym;
    wsym[24] = typesym;
	wsym[25] = untilsym;
	wsym[26] = varsym;
	wsym[27] = whilesym;

    ssym['+'] = plus;
    ssym['-'] = minus;
    ssym['*'] = times;
    ssym['/'] = divsym;
    ssym['['] = lbrack;
    ssym[']'] = rbrack;
    ssym['('] = lparen;
    ssym[')'] = rparen;
    ssym['='] = eql;
    ssym[','] = comma;
    ssym['.'] = period;
    ssym['<'] = lss;
    ssym['>'] = gtr;
    ssym[';'] = semicolon;

    mnemonic[lit] = "LIT  ";
    mnemonic[lod] = "LOD  ";
    mnemonic[sto] = "STO  ";
    mnemonic[cal] = "CAL  ";
    mnemonic[jmp] = "JMP  ";
    mnemonic[jpc] = "JPC  ";
    mnemonic[red] = "RED  ";
    mnemonic[wrt] = "WRT  ";
    mnemonic[ilod] = "ILOD  ";
    mnemonic[loda] = "LODA ";
    mnemonic[lodt] = "LODt  ";
    mnemonic[lodb] = "LODB ";
    mnemonic[cpyb] = "COPYB ";
    mnemonic[endp] = "ENDP ";
    mnemonic[retp] = "RETP  ";
    mnemonic[udis] = "ADIS ";
    mnemonic[mus] = "MUS  ";
    mnemonic[add] = "ADD ";
    mnemonic[sub] = "SUB  ";
    mnemonic[mult] = "MULT ";
    mnemonic[idiv] = "DDIV  ";
    mnemonic[eq] = "EQ  ";
    mnemonic[ne] = "NE ";
    mnemonic[ls] = "LS  ";
    mnemonic[le] = "LE ";
    mnemonic[gt] = "GT  ";
    mnemonic[ge] = "GE ";
    mnemonic[opac] = "OPAC ";
    mnemonic[entp] = "ENTP";
    mnemonic[imod] = "IMOD ";
    mnemonic[ands] = "ANDS";
    mnemonic[ors] = "ORS ";
    mnemonic[nots] = "NOTS";
	mnemonic[opfc] = "OPFC";
	//mnemonic[fret] = "FRET";

    declbegsys = set_of_enum(symbol)::of(constsym, varsym, typesym, procsym,functionsym, eos);
    statbegsys = set_of_enum(symbol)::of(beginsym, callsym, ifsym, whilesym,repeatsym,casesym,forsym, eos);
    facbegsys = set_of_enum(symbol)::of(ident, intcon, lparen, notsym, charcon, eos);
    typebegsys = set_of_enum(symbol)::of(ident, arraysym, eos);
    constbegsys = set_of_enum(symbol)::of(plus, minus, intcon, charcon, ident, eos);
    err = 0;
    a[0] = '\12';// What is \12 LosPhoenix
    display[0] = 0;
    cc = 0;
    cx = 0;
    ll = 0;
    ch = ' ';
    kk = al;
    bx = 1;
    tx = -1;
    lx = 0;
}/*init*/

void enter(alfa x0, oobject x1, types x2, int x3)
{
    tx = tx + 1;
    {
        nametab[tx].name = x0;
        nametab[tx].link = tx - 1;
        nametab[tx].kind = x1;
        nametab[tx].typ = x2;
        nametab[tx].ref = 0;
        nametab[tx].normal = true;
        nametab[tx].lev = 0;
        switch (nametab[tx].kind)
        {
        case variable:
        case prosedure:
		case function:
            nametab[tx].adr = x3;
            break;
        case konstant:
            nametab[tx].val = x3;
            break;
        case typel:
            nametab[tx].size = x3;
            break;
        }
    }
	//output<<tx<<" "<<x0<<" "<<x3<<NL;
}/*enter*/

void enterpreid()// Only at the begining. Init nametable and btab
{
    enter("          ", variable, notyp, 0);    /* sentinel */
    enter("char      ", typel, chars, 1);
    enter("integer   ", typel, ints,  1);
    enter("boolean   ", typel, bool_,  1);
    enter("false     ", konstant, bool_,  0);
    enter("true      ", konstant, bool_,  1);
    enter("read      ", prosedure, notyp, 1);
    enter("write     ", prosedure, notyp, 2);
    btab[0].last = tx;
    btab[0].lastpar = 1;
    btab[0].psize = 0;
    btab[0].vsize = 0;
}/*enterprid*/

void error(int n)
{
	//printf("error: %d\n",n);
    listfile << "****" << format(' ', cc - 1) << '^' << format(n, 2) << NL;
    err = err + 1;
    output << "Error" << n << NL;
}/* error */

void getch()
{
    if (cc == ll)   /* get character to end of line *//* read next line */
    {
        if (eof(sfile))
        {
            output << "program incomplete" << NL;
            close(sfile);
            return;
        }
        ll = 0;
        cc = 0;
        listfile << format(cx, 4) << ' ';   /*print code address */
        while (! eoln(sfile))
        {
            ll = ll + 1;
            sfile >> ch;
            listfile << ch;
            line[ll] = ch;
        }
        listfile << NL;
        sfile >> NL;
        ll = ll + 1;
        line[ll] = ' '; /*process end-line*/
    }
    cc = cc + 1;
    ch = line[cc];
	//printf("%c",ch);
    output << ch;//test
}/* getch */

void getsym()/*getsym*/
{
    int i, k, j;
/*Why is there a goto in this code. LosPhoenix*/
L1:
    while (ch == ' ')
      getch();

    switch (ch)
    {
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
	case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    {
		//output<<" maybeident ";
        /* identifier or reserved word */
        k = 0;
        do
        {
            if (k < al)
            {
                k = k + 1;
                a[k] = ch;
            }
            getch();
        }
        while ((set::of(range('a', 'z'),range('A', 'Z'), range('0', '9'), eos).has(ch)));
        if (k >= kk)
        	  kk = k;    /* kk: last identifier length */
        else
            do
            {
                a[kk] = ' ';
                kk = kk - 1;
            }
            while (!(kk == k));
        id = a;
        i = 1;
        j = norw;  /* binary search reserved word table */
        do
        {
            k = (i + j) / 2;
			//output<<id<<NL;
            if (id <= word1[k])  j = k - 1;
            if (id >= word1[k])  i = k + 1;
			//output<<i<<" "<<j<<NL;
        }
        while (!(i > j));
        if (i - 1 > j)
        	  sym = wsym[k];
        else
        	  sym = ident;
    }
    break;
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
    {
        /* number */
        k = 0;
        num = 0;
        sym = intcon;
        do
        {
            num = 10 * num + (ch - '0');
            k = k + 1;
            getch();
        }
        while ((set::of(range('0', '9'), eos).has(ch)));
        if (k > nmax)  error(47);
    }
    break;
    case ':':
    {
        getch();
        if (ch == '=')
        {
            sym = becomes;
            getch();
        }
        else sym = colon;
    }
    break;
    case '<' :
    {
        getch();
        if (ch == '=')
        {
            sym = leq;
            getch();
        }
        else if (ch == '>')
        {
            sym = neq;
            getch();
        }
        else sym = lss;
    }
    break;
    case '>' :
    {
        getch();
        if (ch == '=')
        {
            sym = geq;
            getch();
        }
        else sym = gtr;
    }
    break;
    case '.' :
    {
        getch();
        if (ch == '.')
        {
            sym = colon;
            getch();
        }
        else   sym = period;
    }
    break;
    case '\''  :
    {
        getch();
        sym = charcon;
        num = ch;
        getch();
        if (ch == '\'')  getch();
        else error(48);
    }
    break;
    case '+':
    case '-':
    case '*':
    case '/':
    case '(':
    case ')':
    case '=':
    case '[':
    case ']':
    case ';':
    case ',':
    {
        sym = ssym[ch];
        getch();
    }
    break;
    default:
    {
        error(0);
        getch();
        goto L1;
    }
    }    /* case */

    output<<sym<<NL;
}/* getsym */

void enterarray (types tp , int l, int h)
{
    if (l > h)  error(14);
    if (ax == amax)
    {
        error(2);
        output << "too many arrays in program " << NL;
        close(sfile);
        close(listfile);
        return;
    }
    else
    {
        ax = ax + 1;
        {
            //A2 &with = atab[ax];

            atab[tx].inxtyp = tp;
            atab[tx].low = l;
            atab[tx].high = h;
        }
    }
}/* enterarray */

void enterblock()
{
    if (bx == bmax)
    {
        error(3);
        output << "too many procedure in program " << NL;
        close(sfile);
        close(listfile);
        return;
    }
    else
    {
        bx = bx + 1;
        btab[bx].last = 0;
        btab[bx].lastpar = 0;
    }
}/* enterblock */

void gen(opcod x, int y, int z)
{
    if (cx > cxmax)
    {
        error(49);
        output << "program too long" << NL;
        close(sfile);
        close(listfile);
        return;
    }
    {
        //instruction &with = code[cx];

        code[cx].f = x;
        code[cx].l = y;
        code[cx].a = z;
    }
    cx = cx + 1;
}/* gen */

void test(symset s1, symset s2, int n)
{
	//printf("begintest\n");
    if (! (s1.has(sym)))
    {
		//printf("error:%d\n",n);
        error(n);
        s1 = s1 + s2;
        while (! (s1.has(sym)))
            getsym();
    }
	//printf("endtest\n");
}/* test */

void enter1( oobject k, int &level)
{
    int j, l;

    if (tx == txmax)
    {
        error(1);
        output << "program too long" << NL;
        close(sfile);
        close(listfile);
        return;
    }
    else
    {
        nametab[0].name = id;
        j = btab[display[level]].last;
        l = j;
        while (nametab[j].name != id)  j = nametab[j].link;
        if (j != 0)
            error(l);
        else
        {
            tx = tx + 1;
            {
                //A1 &with = nametab[tx];

                nametab[tx].name = id;
                nametab[tx].link = l;
                nametab[tx].kind = k;
                nametab[tx].typ = notyp;
                nametab[tx].ref = 0;
                nametab[tx].lev = level;
                nametab[tx].normal = false;
                switch (nametab[tx].kind)
                {
                case variable:
                case prosedure:
				case function:
                    nametab[tx].adr = 0;
                    break;
                case konstant:
                    nametab[tx].val = 0;
                    break;
                case typel:
                    nametab[tx].size = 0;
                    break;
                }    /* initial value */
            }
            btab[display[level]].last = tx;
        }
    }
}/* enter */

int position(alfa id, int &level)
{
    int i, j;

    int position_result;
    nametab[0].name = id;
    j = level;
    do
    {
        i = btab[display[j]].last;
        while (nametab[i].name != id)
            i = nametab[i].link;
        j = j - 1;
    }
    while (!((j < 0) || (i != 0)));
    //if (i == 0)  error(10);
    position_result = i;
    return position_result;
}/* position */

void constant(symset fsys, constrec &c, int &level)
{
    int x, sign;

    c.tp = notyp;
    c.i = 0;
    test(constbegsys, fsys, 50);
    if (constbegsys.has(sym))
    {
        if   (sym == charcon)
        {
            c.tp = chars;
            c.i = num;
            getsym();
        }
        else
        {
            sign = 1;
            if (set_of_enum(symbol)::of(plus, minus, eos).has(sym))
            {
                if (sym == minus)  sign = -1;
                getsym();
            }
            if (sym == ident)
            {
                x = position(id, level);
                if (x != 0){
                    if (nametab[x].kind != konstant)
                        error(12);
                    else
                    {
                        c.tp = nametab[x].typ;
                        c.i = sign * nametab[x].val;
                    }
                }
                getsym();
            }
            else if (sym == intcon)
            {
                c.tp = ints;
                c.i = sign * num;
                getsym();
            }
        }
        test(fsys, set_of_enum(symbol)::of(eos), 6);
    }
}/* constant */

void arraytyp(int &aref, int &arsz, int &level, symset &fsys)
{
    types eltp;
    constrec low, high;
    int elrf, elsz;

    constant(set_of_enum(symbol)::of(colon, rbrack, rparen, ofsym, eos) + fsys, low, level);
    if ((low.tp != ints)  && (low.tp != chars))
        error(50);
    if (sym == colon)  getsym();
    else error(38);
    constant(set_of_enum(symbol)::of(rbrack, comma, rparen, ofsym, eos) + fsys, high, level);
    if (high.tp != low.tp)
    {
        error(40);
        high.i = low.i;
    }
    enterarray(low.tp, low.i, high.i);
    aref = ax;
    if (sym == comma)
    {
        getsym();
        eltp = arrays;
        arraytyp(elrf, elsz, level, fsys);
    }
    else
    {
        if (sym == rbrack)
            getsym();
        else
        {
            error(28);
            if (sym == rparen)  getsym();
        }
        if (sym == ofsym)  getsym();
        else error(17);
        typ(fsys, eltp, elrf, elsz, level);
    }
    {
        //A2 &with = atab[aref];

        arsz = (atab[aref].high - atab[aref].low + 1) * elsz;
        atab[aref].size = arsz;
        atab[aref].eltyp = eltp;
        atab[aref].elref = elrf;
        atab[aref].elsize = elsz;
    }
}/* arraytyp */

void typ(symset fsys, types &tp, int &rf, int &sz, int &level)
{
    types eltp;
    int elrf, x;
    int elsz, offset, t0, t1;
    /* typ */
    tp = notyp;
    rf = 0;
    sz = 0;
    test(typebegsys, fsys, 10);
    if (typebegsys.has(sym))
    {
        if (sym == ident)
        {
            x = position(id, level);
            if (x != 0)
            {
                //A1 &with = nametab[x];
                if (nametab[x].kind != typel)
                    error(19);
                else
                {
                    tp = nametab[x].typ;
                    rf = nametab[x].ref;
                    sz = nametab[x].size;
                    if (tp == notyp)  error(18);
                }
            }
            getsym();
        }
        else if (sym == arraysym)
        {
            getsym();
            if (sym == lbrack)
                getsym();
            else
            {
                error(16);
                if (sym == lparen)
                    getsym();
            }
            tp = arrays;
            arraytyp(rf, sz, level, fsys);
        }
        test(fsys, set_of_enum(symbol)::of(eos), 13);
    }
}/*typ*/

void paramenterlist(symset &fsys, int &level, int &dx)   /*formal parameter list*/
{
    types tp;
    boolean valpar;
    int rf, sz, x, t0;

    getsym();
    tp = notyp;
    rf = 0;
    sz = 0;
    test(set_of_enum(symbol)::of(ident, varsym, eos), fsys + set_of_enum(symbol)::of(rparen, eos), 7);
    while (set_of_enum(symbol)::of(ident, varsym, eos).has(sym))
    {
        if (sym != varsym)
            valpar = true;
        else
        {
            getsym();
            valpar = false;
        }
        t0 = tx;
        if (sym == ident)
        {
            enter1(variable, level);
            getsym();
        }
        else error(22);
        while (sym == comma)
        {
            getsym();
            if (sym == ident)
            {
                enter1(variable, level);
                getsym();
            }
            else error(22);
        }
        if (sym == colon)
        {
            getsym();
            if (sym != ident)
                error(22);
            else
            {
                x = position(id, level);
                getsym();
                if (x != 0)
                {
                    //A1 &with = nametab[x];
                    if (nametab[x].kind != typel)
                        error(19);
                    else
                    {
                        tp = nametab[x].typ;
                        rf = nametab[x].ref;
                        if (valpar)  sz = nametab[x].size;
                        else sz = 1;
                    }
                }
            }
            test (set_of_enum(symbol)::of(semicolon, rparen, eos), set_of_enum(symbol)::of(comma, ident, eos) + fsys, 14);
        }
        else error(24);
        while (t0 < tx)
        {
            t0 = t0 + 1;
            {
                //A1 &with = nametab[t0];

                nametab[t0].typ = tp;
                nametab[t0].ref = rf;
                nametab[t0].adr = dx;
                nametab[t0].lev = level;
                nametab[t0].normal = valpar;
                dx = dx + sz;
            }
        }
        if (sym != rparen)
        {
            if (sym == semicolon)
                getsym();
            else
            {
                error(23);
                if (sym == comma)  getsym();
            }
            test(set_of_enum(symbol)::of(ident, varsym, eos), set_of_enum(symbol)::of(rparen, eos) + fsys, 13);
        }
    }   /*while*/
    if (sym == rparen)
    {
        getsym();
        test(set_of_enum(symbol)::of(semicolon, eos), fsys, 13);
    }
    else error(25);
}/*parameterlist*/

void constdeclaration(int &level, symset &fsys)
{
    constrec c;

    if (sym == ident)
    {
        enter1(konstant, level);
        getsym();
        if (sym == eql)
            getsym();
        else
        {
            error(26);
            if (sym == becomes)  getsym();
        }
        constant(set_of_enum(symbol)::of(semicolon, comma, ident, eos) + fsys, c, level);
        nametab[tx].typ = c.tp;
        nametab[tx].ref = 0;
        nametab[tx].val = c.i;
        if (sym == semicolon)  getsym();
        else error(23);
    }
    else error(22);
    test(fsys + set_of_enum(symbol)::of(ident, eos), set_of_enum(symbol)::of(eos), 13);
}/* constdeclaration */

void typedeclaration(int &level, symset &fsys)
{
    types tp;
    int rf, sz, t1;

    if (sym == ident)
    {
        enter1(typel, level);
        t1 = tx;
        getsym();
        if (sym == eql)
        	  getsym();
        else
        {
            error(26);
            if (sym == becomes)
            	  getsym();
        }
        typ (set_of_enum(symbol)::of(semicolon, comma, ident, eos) + fsys, tp, rf, sz, level);
        nametab[tx].typ = tp;
        nametab[tx].ref = rf;
        nametab[tx].size = sz;
        if (sym == semicolon)  getsym();
        else error(23);
    }
    else error(22);
    test(fsys + set_of_enum(symbol)::of(ident, eos), set_of_enum(symbol)::of(eos), 13);
}/* typedeclaration */

void vardeclaration(int &level, symset &fsys, int &dx)
{
    types tp;
    int t0, t1, rf, sz;

    if (sym == ident)
    {
        t0 = tx;
        enter1(variable, level);
        getsym();
        while (sym == comma)
        {
            getsym();
            if (sym == ident)
            {
                enter1(variable, level);
                getsym();
            }
            else error(22);
        }
        if (sym == colon)
        	  getsym();
        else error(24);
        t1 = tx;
        typ (set_of_enum(symbol)::of(semicolon, comma, ident, eos) + fsys, tp, rf, sz, level);
        while (t0 < t1)
        {
            t0 = t0 + 1;
            {
                //A1 &with = nametab[t0];

                nametab[t0].typ = tp;
                nametab[t0].ref = rf;
                nametab[t0].lev = level;
                nametab[t0].adr = dx;
                nametab[t0].normal = true;
                dx = dx + sz;
            }
        }
        if (sym == semicolon)
        	  getsym();
        else error(23);
    }
    else error(22);
	output<<"varend"<<NL;
    test(fsys + set_of_enum(symbol)::of(ident, eos), set_of_enum(symbol)::of(eos), 13);
}/* vardeclaration */

void procdeclaration(int &level, symset &fsys)
{
    getsym();
    if (sym != ident)
    {
        error(22);
        id = ' ';
    }
    enter1(prosedure, level);
    nametab[tx].normal = true;
    getsym();
    block(set_of_enum(symbol)::of(semicolon, eos) + fsys, level + 1);
    if (sym == semicolon)
    	  getsym();
    else error(23);
}/*procdeclaration*/

void listcode(int &cx0)
{
    int i;

    for( i = cx0; i <= cx - 1; i ++)
    {
        //instruction &with = code[i];
        listfile << format(i, 4) << format(mnemonic[code[i].f], 7) << format(code[i].l, 3) << format(code[i].a, 5) << NL;
    }
}/* listcode */

void factor(symset fsys, item &x, int &level)
{
    int i;
	//output<<"begin factor"<<NL;
    x.typ = notyp;
    x.ref = 0;
    test(facbegsys, fsys, 13);
    if (facbegsys.has(sym))
    {
        switch (sym)
        {
        case ident :
        {
            i = position(id, level);
			output<<i<<NL;
			if(nametab[i].kind==function)
			{
				x.typ=ints;
				//output<<"call statement"<<NL;
				functioncall(level,fsys);
				break;
			}
            getsym();
            if (i == 0)  error(10);
            else
            {
                //A1 &with = nametab[i];
                switch (nametab[i].kind)
                {
                case konstant:
                {
                    x.typ = nametab[i].typ;
                    x.ref = 0;
                    gen(lit, 0, nametab[i].val);
                }
                break;
                case variable:
                {
                    x.typ = nametab[i].typ;
                    x.ref = nametab[i].ref;
                    if ((nametab[i].typ == ints) || (nametab[i].typ == bool_) || (nametab[i].typ == chars))

                        if (nametab[i].normal)   gen(lod, nametab[i].lev, nametab[i].adr);
                        else gen(ilod, nametab[i].lev, nametab[i].adr);
                    else  if (nametab[i].typ == arrays)
                    {
                        if (nametab[i].normal)  gen(loda, nametab[i].lev, nametab[i].adr);
                        else gen(lod, nametab[i].lev, nametab[i].adr);
                        if (sym == lbrack)
                            arrayelement(fsys, x, level); //LosPhoenix tocheck
                        if (x.typ != arrays)
                            gen(lodt, 0, 0);
                    }
                }
                break;
                case prosedure:
                case typel:
                    error(41);
                    break;
                }
            }
        }
        break;
        case intcon:
        case charcon :
        {
            if (sym == intcon)  x.typ = ints;
            else x.typ = chars;
            x.ref = 0;
            gen(lit, 0, num);
            getsym();
        }
        break;
        case lparen :
        {
            getsym();
            expression(set_of_enum(symbol)::of(rparen, eos) + fsys, x, level);
            if (sym == rparen)  getsym();
            else error(25);
        }
        break;
        case notsym :
        {
            getsym();
            factor(fsys, x, level);
            if (x.typ == bool_)
                gen(nots , 0, 0);
            else error(43);
        }
        break;
        }    /* case */
        test(fsys + set_of_enum(symbol)::of(rbrack, rparen, eos), facbegsys, 23);
    }    /* of if */
}/* factor */

void term(symset fsys, item &x, int &level)
{
	//output<<"begin term"<<NL;
    symbol mulop;
    item y;
    /* term */
    factor(fsys + set_of_enum(symbol)::of(times, divsym, modsym, andsym, eos), x, level);
    while (set_of_enum(symbol)::of(times, divsym, modsym, andsym, eos).has(sym))
    {
        mulop = sym;
        getsym();
        factor(fsys + set_of_enum(symbol)::of(times, divsym, modsym, andsym, eos), y, level);
        if (x.typ != y.typ)
        {
            error(40);
            x.typ = notyp;
            x.ref = 0;
        }
        else
        {
            if (mulop == times)
                if (x.typ == ints)
                    gen(mult, 0, 0);
                else error(43);
            if  (mulop == divsym)
                if (x.typ == ints)
                    gen(idiv, 0, 0);
                else error(43);
            if  (mulop == modsym)
                if (x.typ == ints)
                    gen(imod, 0, 0);
                else error(43);
            if  (mulop == andsym)
                if (x.typ == bool_)
                    gen(ands, 0, 0);
                else error(43);
        }
    }
}/* term*/

void simpleexpression(symset fsys, item &x, int &level)
   {
	   //output<<"begin simple expression"<<NL;
    symbol addop;
    item y;
    /* simpleexpression */
    if (set_of_enum(symbol)::of(plus, minus, eos).has(sym))
    {
        addop = sym;
        getsym();
        term(fsys + set_of_enum(symbol)::of(plus, minus, orsym, eos), x, level);
        if (addop == minus)  gen(mus, 0, 0);
    }
    else term(fsys + set_of_enum(symbol)::of(plus, minus, orsym, eos), x, level);
    while (set_of_enum(symbol)::of(plus, minus, orsym, eos).has(sym))
    {
        addop = sym;
        getsym();
        term(fsys + set_of_enum(symbol)::of(plus, minus, orsym, eos), y, level);
        if (x.typ != y.typ)
        {
            error(40);
            x.typ = notyp;
            x.ref = 0;
        }
        else
        {
            if (addop == plus)
                if (x.typ == ints)
                    gen(add, 0, 0);
                else error(43);
            if  (addop == minus)
                if (x.typ == ints)
                    gen(sub, 0, 0);
                else error(43);
            if  (addop == orsym)
                if (x.typ == bool_)
                    gen(ors, 0, 0);
                else error(43);
        }
    }
}/* simpleexpression */

void expression(symset fsys, item &x, int &level)
{
    symbol relop;
    item y;
    /*expression*/
    simpleexpression(set_of_enum(symbol)::of(eql, neq, lss, gtr, leq, geq, eos) + fsys, x, level);
    while (set_of_enum(symbol)::of(eql, neq, lss, leq, gtr, geq, eos).has(sym))
    {
		//output<<"begin express"<<NL;
        relop = sym;
        getsym();
        simpleexpression(fsys, y, level);
        if (x.typ != y.typ)
            error(40);
        switch (relop)
        {
        case eql:
            gen(eq, 0, 0);
            break;
        case neq:
            gen(ne, 0, 0);
            break;
        case lss:
            gen(ls, 0, 0);
            break;
        case geq:
            gen(ge, 0, 0);
            break;
        case gtr:
            gen(gt, 0, 0);
            break;
        case leq:
            gen(le, 0, 0);
            break;
        }
        x.typ = bool_;
    }
}/* expression */

void arrayelement(symset fsys, item &x, int &level)
{
    int cc;
    index1 addr, p;
    item y;

    p = x.ref;
    if (sym == lbrack)
    {
        do
        {
            getsym();
            expression(fsys + set_of_enum(symbol)::of(comma, eos), y, level);
            if (x.typ != arrays)  error(40);
            else
            {
                if (y.typ != atab[p].inxtyp)  error(44);
                gen(lit, 0, atab[p].low);
                gen(sub, 0, 0);
                gen(lit, 1, atab[p].elsize);
                gen(mult, 0, 0);
                gen(add, 0, 0);
                x.typ = atab[p].eltyp;
                x.ref = atab[p].elref;
                p = atab[p].elref;
            }
        }
        while (!(sym != comma));

        if (sym == rbrack)  getsym();
        else error(28);
    }
    else error(16);
    test(fsys, set_of_enum(symbol)::of(eos), 13);
}/*arrayelement*/

void assignment(int &i, int &level, symset &fsys)
{
    item x, y;

    i = position(id, level);
    if (i == 0)  error(10);
    else if (nametab[i].kind != variable&&nametab[i].kind != function)
    {
        error(30);
        i = 0;
    }
	x.typ = nametab[i].typ;
	x.ref = nametab[i].ref;
    getsym();
	if(nametab[i].kind == variable)
	{
        A1 &with = nametab[i];
        if (with.normal)
            gen(loda, with.lev, with.adr);
        else gen(lod, with.lev, with.adr);
    }
	else
	{
		gen(loda, level, -1);
	}
    if (sym == lbrack)
        arrayelement(fsys + set_of_enum(symbol)::of(becomes, eos), x, level);
    if (sym == becomes)  getsym();
    else
    {
        error(33);
        if (sym == eql)  getsym();
    }
    expression(fsys, y, level);
	output<<x.typ<<" "<<y.typ<<NL;
    if (x.typ != y.typ)  error(40);
    else if (x.typ == arrays)
        if (x.ref == y.ref)
            gen(cpyb, 0, atab[x.ref].size);
        else error(40);
    else gen(sto, 0, 0);
}/*assignment*/

void ifstatement(int &level, symset &fsys, int &cx1, int &cx2)
{
    item x;

    getsym();
    expression(set_of_enum(symbol)::of(thensym, dosym, eos) + fsys, x, level);
    if (x.typ != bool_)  error(34);
    if (sym == thensym)  getsym();
    else error(35);
    cx1 = cx;
    gen(jpc, 0, 0);
    statement(fsys + set_of_enum(symbol)::of(elsesym, eos), level);
    if (sym == elsesym)
    {
        getsym();
        cx2 = cx;
        gen(jmp, 0, 0);
        code[cx1].a = cx;
        labtab[lx] = cx;
        lx = lx + 1;
        statement(fsys, level);
        code[cx2].a = cx;
        labtab[lx] = cx;
        lx = lx + 1;
    }
    else
    {
        code[cx1].a = cx;
        labtab[lx] = cx;
        lx = lx + 1;
    }
}/*ifstatement*/

void compound(symset &fsys, int &level)
{
    getsym();
    statement(set_of_enum(symbol)::of(semicolon, endsym, eos) + fsys, level);
    while ((set_of_enum(symbol)::of(semicolon, eos) + statbegsys).has(sym))
    {
        if (sym == semicolon)  getsym();
        else error(23);
        statement(set_of_enum(symbol)::of(semicolon, endsym, eos) + fsys, level);
    }
    if (sym == endsym)
        getsym();
    else error(36);
}/*compound*/

void whilestatement(int &cx1, int &level, symset &fsys, int &cx2)
{
    item x;

    getsym();
	printf("%d,%d\n",lx,cx);
    labtab[lx] = cx;
    lx = lx + 1;
    cx1 = cx;
	printf("%d\n",cx);
    expression(set_of_enum(symbol)::of(dosym, eos) + fsys, x, level);
    if (x.typ != bool_)  error(34);
    cx2 = cx;
	printf("%d\n",cx);
    gen(jpc, 0, 0);
    if (sym == dosym)  getsym();
    else error(37);
    statement(fsys, level);
    gen(jmp, 0, cx1);
    code[cx2].a = cx;
	printf("%d,%d\n",lx,cx);
    labtab[lx] = cx;
    lx = lx + 1;
}/*whilestatement*/

void stanproc(int i, int &level, item &x, symset &fsys)
{
    int n;

    if (i == 6)
    {
        /* read */
        getsym();
        if (sym == lparen)
        {
            do
            {
                getsym();
                if (sym == ident)
                {
                    n = position(id, level);
                    getsym();
                    if (n == 0)  error(10);
                    else if (nametab[n].kind != variable)
                    {
                        error(30);
                        n = 0;
                    }
                    else
                    {
                        x.typ = nametab[n].typ;
                        x.ref = nametab[n].ref;
                        if (nametab[n].normal)
                            gen(loda, nametab[n].lev, nametab[n].adr);
                        else gen(lod, nametab[n].lev, nametab[n].adr);
                        if (sym == lbrack)
                            arrayelement(fsys + set_of_enum(symbol)::of(comma, eos), x, level);
                        if (x.typ == ints)
                            gen(red, 0, 0);
                        else if (x.typ == chars)
                            gen(red, 0, 1);
                        else error(43);
                    }
                }
                else error(22);
            }
            while (!(sym != comma));
            if (sym != rparen)  error(25);
            else  getsym();
        }
        else error(32);
    }
    else if (i == 7)
    {
        /* write */
        getsym();
        if (sym == lparen)
        {
            do
            {
                getsym();
                expression(set_of_enum(symbol)::of(rparen, comma, eos) + fsys, x, level);
                if (x.typ == ints)
                    gen(wrt, 0, 0);
                else if (x.typ == chars)
                    gen(wrt, 0, 1);
                else error(43);
            }
            while (!(sym != comma));
            if (sym != rparen)  error(25);
            getsym();
        }
        else error(32);
    }
}/* standproc */
void functioncall(int &level,symset &fsys)
{
	i=position(id,level);
	if(i==0)
	{
		output<<"function not found"<<NL;
	}
	gen(opfc,0,0);
	getsym();
	do{
		getsym();
		item x;
		expression(fsys + set_of_enum(symbol)::of(comma, colon, rparen, eos), x, level);
	}while(sym==comma);
	getsym();
	gen(cal,nametab[i].lev-1,nametab[i].adr);
	if(nametab[i].adr<=0) nametab[i].adr=-(cx-1);
	if(level>=nametab[i].lev) gen(udis,nametab[i].lev,level);
}

void call(int &level, symset &fsys)
{
    item x;
    int lastp, cp, i, j, k;
	//output<<"function call"<<NL;
    /* call */
    //getsym();
    if (sym == ident)
    {
        i = position(id, level);
        if (nametab[i].kind == prosedure||nametab[i].kind == function)
        {
            if (nametab[i].lev == 0)  stanproc(i, level, x, fsys);
            else
            {
                getsym();
				if(nametab[i].kind == function) gen(opfc, 0, 0);
				else gen(opac, 0, 0); /*open active record*/
                lastp = btab[nametab[i].ref].lastpar;
                if(nametab[i].kind==prosedure) cp = i;
				//else cp=lastp-btab[nametab[i].ref].psize+3;
                if (sym == lparen)
                {
                    /*actual parameter list*/
                    do
                    {
                        getsym();
                        if (cp >= lastp)
						{
							//output<<cp<<" "<<lastp<<NL;
                            error(29);
						}
                        else
                        {
                            cp = cp + 1;
                            if (nametab[cp].normal)
                            {
                                /*value parameter*/
                                expression(fsys + set_of_enum(symbol)::of(comma, colon, rparen, eos), x, level);
                                if (x.typ == nametab[cp].typ)
                                {
                                    if (x.ref != nametab[cp].ref)
                                        error(31);
                                    else if (x.typ == arrays)
                                        gen(lodb, 0, atab[x.ref].size);
                                }
                                else error(31);
                            }
                            else           /*variable parameter*/
                            {
                                if (sym != ident)
                                    error(22);
                                else
                                {
                                    k = position(id, level);
                                    getsym();
                                    if (k != 0)
                                    {
                                        if (nametab[k].kind != variable)  error (30);
                                        x.typ = nametab[k].typ;
                                        x.ref = nametab[k].ref;
                                        if (nametab[k].normal)
                                            gen(loda, nametab[k].lev, nametab[k].adr);
                                        else gen(lod, nametab[k].lev, nametab[k].adr);
                                        if (sym == lbrack)
                                            arrayelement(fsys + set_of_enum(symbol)::of(comma, rparen, eos), x, level);
                                        if    ((nametab[cp].typ != x.typ)
                                                || (nametab[cp].ref != x.ref))
                                            error(31);
                                    }
                                }
                            }   /*variable parameter*/
                        }
                        test(set_of_enum(symbol)::of(comma, rparen, eos), fsys, 13);
                    }
                    while (!(sym != comma));
                    if (sym == rparen)  getsym();
                    else error(25);
                }
                if (cp < lastp)  error(39);  /*too few actual parameters*/
                gen(cal, nametab[i].lev, nametab[i].adr);
                if (nametab[i].lev < level)  gen(udis, nametab[i].lev, level);
            }
        }
        else error(51);
    }
    else error(22);
    test(fsys + set_of_enum(symbol)::of(ident, eos), set_of_enum(symbol)::of(eos), 13);
	//printf("callend\n");
} /*call*/

void fsplit(const char sfilename[], char* dir, char* name, char* ext)
{
    //Todo LosPhoenix
/*
    char path_buffer = sfilename.get_body();
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];
*/
    char drive[_MAX_DRIVE];
    char dir_temp[_MAX_DIR];
    _splitpath(sfilename, drive, dir_temp, name, ext);
    _makepath( dir, drive, dir_temp, "", "");

}

/*********************************************************/
//writeen by wwt

void repeatstatement(int &cx1, int &level, symset &fsys, int &cx2)
{
    item x;

	getsym();
	labtab[lx] = cx;
    lx = lx + 1;
    cx1 = cx;
	 while(1){
	statement(set_of_enum(symbol)::of(untilsym, eos)+fsys, level);
	if(sym==semicolon) getsym();
	else if (sym==untilsym) break;
	//system("pause");
	}
    //else error(37);
	getsym();
	expression(fsys, x, level);
	//if (x.typ != bool_)  error(34);
	cx2=cx;
	gen(jpc,0,cx1);
	labtab[lx] = cx;
	lx=lx+1;
}/*repeatstatement*/


void casestatement(int &cx1, int &level, symset &fsys, int &cx2)
{
    item x,y;
	int temp = lx;
	lx+=1;
	gen(loda,level,temp);
    getsym();
	expression(fsys+set_of_enum(symbol)::of(ofsym, eos), x, level);
	gen(sto,0,0);
	int caseadr=0;//case backpatch
	output<<"casestart"<<NL;
	int mid=0;
	do
	{
		if(mid) code[mid].a=cx;
		int listadr=0;//list backpatch
		output<<"liststart"<<NL;
		do
		{
			getsym();
			factor(fsys+set_of_enum(symbol)::of(comma,colon, eos),y,level);
			gen(lod,level,temp);
			gen(ne,0,0);
			gen(jpc,0,listadr);
			listadr=cx-1;
		}
		while(sym==comma);
		output<<"listend"<<NL;
		mid=cx;
		gen(jmp,0,0);
		getsym();
		cx1=cx;
		while(listadr)
		{
			int mid1=code[listadr].a;
			code[listadr].a=cx1;
			listadr=mid1;
		}
		statement(fsys+set_of_enum(symbol)::of(semicolon,endsym, eos),level);
		gen(jmp,0,caseadr);
		caseadr=cx-1;
	}
	while(sym==semicolon);
	cx2=cx;
	while(caseadr)
	{
		int mid1=code[caseadr].a;
		code[caseadr].a=cx2;
		caseadr=mid1;
	}
	code[mid].a=cx;
	output<<"caseend"<<NL;
	getsym();

}/*casestatement*/


void forstatement(int &cx1, int &level, symset &fsys, int &cx2)
{
    item x,y;
	int i;
	bool isadd;
	int tx1=0;
	getsym();
	//assignment(i,level,fsys);
	i=position(id, level);
	getsym();
	A1 &temp1 = nametab[i];
	gen(loda,level,temp1.adr);
	getsym();
	//output<<"startexpression"<<NL;
	expression(fsys+set_of_enum(symbol)::of(downtosym,tosym, eos), x, level);
	gen(sto,0,0);
	//output<<"assignmentend"<<NL;
	//set_of_enum(symbol)::of(tosym, eos)+set_of_enum(symbol)::of(downtosym, eos)+
	if(sym==tosym) isadd=1;
	else if(sym==downtosym) isadd=0;
	getsym();
	int temp2 = lx;
	lx+=1;
	gen(loda,level,temp2);
	expression(set_of_enum(symbol)::of(dosym, eos)+fsys,y,level);
	getsym();
	gen(sto,0,0);//temp2=expression
	gen(lod,level,temp1.adr);//cmp
	gen(lod,level,temp2);
	if(isadd) gen(le,0,0);
	else gen(ge,0,0);
	gen(jpc,0,0);//cx1-1
	cx1=cx;//body
	statement(fsys,level);
	gen(lod,level,temp1.adr);//cmp
	gen(lod,level,temp2);
	gen(ne,0,0);
	gen(jpc,0,0);
	gen(loda,level,temp1.adr);//inc
	gen(lod,level,temp1.adr);
	gen(lit,0,1);
	if(isadd) gen(add,0,0);
	else gen(sub,0,0);
	gen(sto,0,0);
	gen(jmp,0,cx1);//back to body
	cx2=cx;
	code[cx1-1].a=cx2;
	code[cx2-7].a=cx2;
    return;
}/*forstatement*/
//END
/******************************************************/
void statement(symset fsys, int &level)
{
	//printf("beginstatement\n");
    int i, cx1, cx2, cx3;
    item x;
    void arrayelement(symset fsys, item & x);
    /* statement */
    test(statbegsys + set_of_enum(symbol)::of(ident, eos), fsys, 13);
    if (sym == ident)   assignment(i, level, fsys);
    else if (sym == callsym){
		getsym();
		//printf("callstatement\n");
        call(level, fsys);
		//
		}
    else if (sym == ifsym){
		//printf("ifstatement\n");
        ifstatement(level, fsys, cx1, cx2);
		//
		}
    else if (sym == beginsym){
		//printf("compound\n");
        compound(fsys, level);
		//
		}
    else if (sym == whilesym){//
		//printf("whilestatement\n");
        whilestatement(cx1, level, fsys, cx2);
		}
	else if (sym==forsym){
		//printf("forstatement\n");
		forstatement(cx1, level, fsys, cx2);
		//
		}
	else if (sym==repeatsym){
		//printf("repeatstatement\n");
		repeatstatement(cx1, level, fsys, cx2);
		//printf("endrepeatstatement\n");
		//
		}
	else if (sym==casesym){
		//printf("casestatement\n");
		casestatement(cx1, level, fsys, cx2);
	//
	}
    test(fsys + set_of_enum(symbol)::of(elsesym, eos), set_of_enum(symbol)::of(eos), 13);
	//printf("endstatement\n");
}/* statement */

void funcdeclaration(int level,symset fsys)
{
	getsym();
	int prt=position(id,level);
	if(!prt)
	{
		enter(id,function,ints,0);
		btab[display[level]].last=tx;
		nametab[tx].lev=level+1;
		prt=tx;
	}
	array<0,100,alfa> varlist;
	int idnum=0;
	getsym();
	do
	{
		getsym();
		varlist[idnum]=id;
		getsym();
		idnum++;
	}while(sym==comma);
	//output<<"  idend   "<<NL;
	do getsym(); while(sym!=semicolon);
	getsym();
	//output<<"look if forward"<<NL;
	if(sym==forwardsym) //forward
	{
		getsym();
		getsym();
		output<<"function end"<<NL;
		return;
	}
	level++;
	int dx=3;//begin
	enterblock();
	display[level]=bx;
	nametab[prt].ref=bx;
	for(int i=0;i<idnum;i++)
	{
		enter(varlist[i],variable,ints,dx);
		if(i==0) nametab[tx].link=0;
		nametab[tx].lev=level;
		//output<<varlist[i]<<NL;
		dx=dx+1;
	}//enter par to nametab
	while(nametab[prt].adr<0)
	{
		int mid=nametab[prt].adr;
		nametab[prt].adr=code[-mid].a;
		code[-mid].a=cx;
	}//backpatch cx
	nametab[prt].adr=cx;
	int prb=bx;
	btab[prb].lastpar=tx;
	btab[prb].last=tx;
	btab[prb].psize=dx;
	btab[prb].vsize=dx;//newbtab
	output<<level<<NL;
	output<<position(varlist[0],level)<<" "<<position(varlist[1],level)<<NL;
	getsym();
	output<<level<<" "<<btab[display[level]].last<<NL;
	gen(entp,level,btab[prb].vsize);
	statement(set_of_enum(symbol)::of(semicolon, endsym, eos) + fsys, level);
	//output<<"function end"<<NL;
	gen(retp,0,0);
	getsym();
	getsym();
	getsym();
}
	

void block( symset fsys, int level)
{
    int dx;  /* data allocation index */
    int tx0; /* initial table index */
    int cx0; /* initial code  index */
    int prt, prb;
    /* block */
    prt = tx;
    dx = 3;
    tx0 = tx;
    nametab[tx].adr = cx;
    if (level > levmax)  error(4);
    enterblock();
    prb = bx;
    display[level] = bx;
    nametab[prt].typ = notyp;
    nametab[prt].ref = prb;
	int oep=0;

    if((sym == lparen) && (level > 1))

    {
        paramenterlist(fsys, level, dx);
        if (sym == semicolon)  getsym();
        else error(23);
    }
    else  if (level > 1)
        if (sym == semicolon)  getsym();
        else error(23);
    btab[prb].lastpar = tx;
    btab[prb].psize = dx;
	//printf("start poc\n");
	//oep=cx;
	//output<<"oep"<<cx<<NL;
    gen(jmp, 0, 0); /* jump from declaration part to statement part */
    do
    {
		output<<"test NOW"<<NL;
        if (sym == constsym)
        {
            getsym();
            do
            {
                constdeclaration(level, fsys);
            }
            while (!(sym != ident));
        }
        if (sym == typesym)
        {
            getsym();
            do
            {
                typedeclaration(level, fsys);
            }
            while (!(sym != ident));
        }
        if (sym == varsym)
        {
            getsym();
            do
            {
                vardeclaration(level, fsys, dx);
            }
            while (!(sym != ident));
        }
        while (sym == procsym)    procdeclaration(level, fsys);
		while(sym==functionsym)   funcdeclaration(level, fsys);
        test(statbegsys + set_of_enum(symbol)::of(ident, eos), declbegsys, 13);
    }
    while (!(! (declbegsys.has(sym))));
	/*while(sym==functionsym)
	{
		funblock(set_of_enum(symbol)::of(semicolon, beginsym, eos)+fsys,level+1);
	}*/
    code[nametab[tx0].adr].a = cx; /*back enter statement code's start adr. */
    labtab[lx] = cx;
    lx = lx + 1;
    {
        //A1 &with = nametab[tx0];

        nametab[tx0].adr = cx; /*code's start address */
    }
    cx0 = cx;
	//code[oep].a=cx0;
    gen(entp, level, dx); /* block entry */
	output<<"enter main "<<level<<NL;
    statement(set_of_enum(symbol)::of(semicolon, endsym, eos) + fsys, level);
	//printf("su4\n");
    if (level > 1)  gen(retp, 0, 0); /*return*/
    else gen(endp, 0, 0); /* end prograam */
    test(fsys, set_of_enum(symbol)::of(eos), 13);
    listcode(cx0);
	//printf("end\n");
}/* block */


/************************************************************************/
int main(int argc, const char *argv[])
{
    /* main */
    pio_initialize(argc, argv);
    output << "Please input source program file name:" << NL;
    input >> sfile_buff >> NL;
    strcpy(sfilename, sfile_buff.get_body());
	//printf("suc1\n");
    output << sfilename << NL;

    assign(sfile, sfilename);
    reset(sfile);

    fsplit(sfilename, dir, name, ext);
    listfile_buff = strcat(dir, strcat(name, "c.lst"));
    assign(listfile, listfile_buff);
    rewrite(listfile);
	//printf("suc2\n");
    initial();
    enterpreid();
    getsym();
	//printf("suc3\n");
    if (sym == programsym)
    {
        getsym();
        if (sym == ident)
        {
            getsym();
            if (sym == semicolon)  getsym();
            else error(23);
        }
        else error(22);
    }
    else error(15);
	//printf("suc4\n");
    test(declbegsys + set_of_enum(symbol)::of(beginsym, eos), set_of_enum(symbol)::of(eos), 13);
    block(set_of_enum(symbol)::of(period, eos) + declbegsys + statbegsys, 1);
	//printf("suc5\n");
    if (sym != period)  error(38);
	for(int iden=0;iden<=tx;iden++)
	{
		output<<iden<<" "<<nametab[iden].name<<" kind: "<<nametab[iden].kind<<" lev: "<<nametab[iden].lev<<" typ: "<<nametab[iden].typ<<" norm: "<<nametab[iden].normal<<" ref: "<<nametab[iden].ref<<" adr: "<<nametab[iden].adr<<" "<<nametab[iden].link<<NL;
	//printf("suc5\n");
	}
	output<<NL;
	for(int iden=0;iden<=bx;iden++)
	{
		output<<iden<<" "<<btab[iden].last<<" "<<btab[iden].lastpar<<" "<<btab[iden].psize<<" "<<btab[iden].vsize<<" "<<NL;
	//printf("suc5\n");
	}
    if (err == 0)
    {
        output << "SUCCESS";
        fsplit(sfilename, dir, name, ext);
        assign(fcode, strcat(dir, strcat(name , "c.pld")));
        rewrite(fcode);
        for( i = 0; i <= cx; i ++)
            fcode << code[i];
        close(fcode);
        fsplit(sfilename, dir, name, ext);
        assign(labfile, strcat(dir, strcat(name ,"c.lab")));
        rewrite(labfile);
        for( i = 0; i <= lx; i ++)
            labfile << labtab[i];
        close(labfile);
    }
    else output << err << "ERRORS IN PROGRAM";
	//printf("suc6\n");
    output << NL;
    close(sfile);
    close(listfile);
    return EXIT_SUCCESS;
}     /* of whole program  */

#ifndef WIN32
void _makepath(char *path,const char *drive, char *dir, char *fname, char *ext)
{
	strcpy(path,dir);
}

void _splitpath(const char *path, char *drive, char *dir, char *fname, char *ext)
{
	const char *p_whole_name;
 
	drive[0] = '\0';
	if (NULL == path)
	{
		dir[0] = '\0';
		fname[0] = '\0';
		ext[0] = '\0';
		return;
	}
 
	if ('/' == path[strlen(path)])
	{
		strcpy(dir, path);
		fname[0] = '\0';
		ext[0] = '\0';
		return;
	}
 
	p_whole_name = rindex(path, '/');
	if (NULL != p_whole_name)
	{
		p_whole_name++;
		_split_whole_name(p_whole_name, fname, ext);
 
		snprintf(dir, p_whole_name - path, "%s", path);
	}
	else
	{
		_split_whole_name(path, fname, ext);
		dir[0] = '\0';
	}
}
 
static void _split_whole_name(const char *whole_name, char *fname, char *ext)
{
	const char *p_ext;
 
	p_ext = rindex(whole_name, '.');
	if (NULL != p_ext)
	{
		strcpy(ext, p_ext);
		snprintf(fname, p_ext - whole_name + 1, "%s", whole_name);
	}
	else
	{
		ext[0] = '\0';
		strcpy(fname, whole_name);
	}
}
 
#endif

