#include "ptoc.h"
#include<stdlib.h>


const integer stacksize = 1023;
const integer cxmax = 200;
const integer levmax = 10;
const integer amax = 2047;
enum types {notyp, ints, chars, arrays, last_types};
enum opcod {lit, lod, ilod, loda, lodt, sto, lodb, cpyb, jmp, jpc, red, wrt,
            cal, retp, endp, udis, opac, entp, ands, ors, nots, imod, mus, add,
            sub, mult, idiv, eq, ne, ls, le, gt, ge, opfc,last_opcod
           };  /* opration code */
struct instruction
{
    opcod f;
    unsigned char l;
    short a;
};
integer pc, base, top,eax; /* program-,base-,topstack-register */
integer oldtop;
instruction i;          /* instruction register */

array<0, stacksize, integer> s;                     /* data store */

array<0, levmax, integer> display;
array<0, cxmax, instruction> code;
file<instruction> fcode;
array<0, last_opcod, varying_string<5> > mnemonic;
string filecode;            /* name of code file */
boolean stop;
integer h1, h2, h3;
char ch;
void load()
{
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
    integer i;
	//output <<sizeof(instruction)<<NL;
    output << "please input code file:" << NL;
    input >> filecode;
    assign(fcode, filecode);
    reset(fcode);
    i = 0;
    while (! eof(fcode))
    {
        fcode >> code[i];
        i = i + 1;
    }
    close(fcode);
}
int main(int argc, const char *argv[])
{
    /* main */
    pio_initialize(argc, argv);
    load();
    output << "START PL/0" << NL;
    oldtop = 0;
    stop = false;
    top = 0;
    base = 0;
    pc = 0;
    display[1] = 0;
    s[1] = 0;
    s[2] = 0;
    s[3] = 0;
	//output<<"    code\ti\ta\tesp\tebp\teip  "<<NL;
    do
    {
        i = code[pc];
        pc = pc + 1;
		//output <<pc<<"\t"<<mnemonic[i.f] << "\t"<<(int) i.l <<"\t"<<(int)i.a<<NL;
        switch (i.f)
        {
        case lit:
        {
            top = top + 1;
            s[top] = i.a;
        }
        break;
        case lod:
        {
            top = top + 1;
            s[top] = s[display[i.l] + i.a];
        }
        break;
        case loda:
        {
            top = top + 1;
            s[top] = display[i.l] + i.a;
        }
        break;
        case ilod:
        {
            top = top + 1;
            s[top] = s[s[display[i.l] + i.a]];
        }
        break;
        case lodt:
        {
            s[top] = s[s[top]];
        }
        break;
        case lodb:
        {
            h1 = s[top];
            top = top - 1;
            h2 = i.a + top;
            while (top < h2)
            {
                top = top + 1;
                s[top] = s[h1];
                h1 = h1 + 1;
            }
        }
        break;
        case cpyb:
        {
            h1 = s[top - 1];
            h2 = s[top];
            h3 = h1 + i.a;
            while (h1 < h3)
            {
                s[h1] = s[h2];
                h1 = h1 + 1;
                h2 = h2 + 1;
            }
            top = top - 2;
        }
        break;
        case sto:
        {
            s[s[top - 1]] = s[top];
            top = top - 2;
        }
        break;
        case opac:
        {
            oldtop = top;
            top = top + 3;
        }
        break;
		case opfc://change for func
		{
			oldtop = top +1;
            top = top + 4;
		}
		break;
        case cal:    /* generate new block mark */
        {
            s[oldtop + 1] = pc;
            s[oldtop + 2] = display[i.l];
            s[oldtop + 3] = base;
            pc = i.a;
        }
        break;
        case entp:
        {
            base = oldtop + 1;
            display[i.l] = base;
            top = oldtop + i.a;
        }
        break;
        case udis:
        {
            h1 = i.a;
            h2 = i.l;
            h3 = base;
			//output<<h1<<" "<<h2<<" "<<h3<<NL;
			if((int)h1==(int)h2)
			{
				//output<<"h1==h2"<<NL;
				display[h1]=base;
				break;
			}
            do
            {
                display[h1] = h3;
                h1 = h1 - 1;
                h3 = s[h3 + 2];
            }
            while (!(h1 == h2));
        }
        break;
        case jmp:
            pc = i.a;
            break;
        case jpc:
        {
            if (s[top] == 0)  pc = i.a;
            top = top - 1;
        }
        break;
        case retp:  /*return*/
        {
            top = base - 1;
            pc = s[top + 1];
            base = s[top + 3];
        }
        break;
        case endp:
        {
            stop = true;
        }
        break;
        case red:
        {
            output << " ??:";
            if (i.a == 0)  input >> s[s[top]] >> NL;
            else
            {
                input >> ch >> NL;
                s[s[top]] = ch;
            }
            top = top - 1;
        }
        break;
        case wrt:
        {
            if (i.a == 0)  output << s[top] << NL;
            else
            {
                ch = s[top];
                output << ch << NL;
            }
            top = top - 1;
        }
        break;
        case mus :
            s[top] = -s[top];
            break;
        case add :
        {
            top = top - 1;
            s[top] = s[top] + s[top + 1];
        }
        break;
        case sub :
        {
            top = top - 1;
            s[top] = s[top] - s[top + 1];
        }
        break;
        case mult:
        {
            top = top - 1;
            s[top] = s[top] * s[top + 1];
        }
        break;
        case idiv:
        {
            top = top - 1;
            s[top] = s[top] / s[top + 1];
        }
        break;
        case imod:
        {
            top = top - 1;
            s[top] = s[top] % s[top + 1];
        }
        break;
        case ands:
        {
            top = top - 1;
            s[top] = s[top] & s[top + 1];
        }
        break;
        case ors :
        {
            top = top - 1;
            s[top] = s[top] |  s[top + 1];
        }
        break;
        case nots:
            s[top] = ~ s[top];
            break;
        case eq  :
        {
            top = top - 1;
            s[top] = (s[top] == s[top + 1]);
        }
        break;
        case ne  :
        {
            top = top - 1;
            s[top] = (s[top] != s[top + 1]);
        }
        break;
        case ls  :
        {
            top = top - 1;
            s[top] = (s[top] < s[top + 1]);
        }
        break;
        case ge  :
        {
            top = top - 1;
            s[top] = (s[top] >= s[top + 1]);
        }
        break;
        case gt  :
        {
            top = top - 1;
            s[top] = (s[top] > s[top + 1]);
        }
        break;
        case le  :
        {
            top = top - 1;
            s[top] = (s[top] <= s[top + 1]);
        }
        break;
        }    /* case,with */
		/*
		output<<top<<"\t"<<base<<"\t"<<NL;
		output<<display[1]<<" "<<display[2]<<NL;
		
		for(int i=0;i<=top;i++)
		{
			output<<i<<":"<<s[i]<<"  ";
		}
		output<<NL;*/
		//system("pause");
    }
    while (!(stop == true));
    output << " END PL/0 " << NL;
    return EXIT_SUCCESS;
}      /* interpret */
