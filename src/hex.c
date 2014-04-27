/**
 * Written by: Mehdi Sotoodeh
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ''AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>

#define H_BYTE          0
#define H_WORD          1
#define H_DWORD         2

#define STYLE_NONE      0
#define STYLE_ASM       1
#define STYLE_C         2
#define STYLE_JAVA      3
#define STYLE_TEXT      4

#define P_OFFSET        0x01
#define P_TEXT          0x02

char * array_name = "xxxx";
char * storage_type_c[] = { "char", "short", "long" };
char * storage_type_asm[] = { "byte", "word", "dword" };
char * storage_type_java[] = { "byte", "short", "int" };
int  default_line_bytes_c[] = { 8, 16, 24 };
int  default_line_bytes_asm[] = { 16, 16, 24 };
int  default_line_bytes_java[] = { 8, 16, 16 };
int  big_endian = 0;

FILE * outfile;

unsigned long get_word( unsigned char * buff )
{
    if( big_endian )
    {
        return  ((unsigned long)buff[0] << 8) | 
                ((unsigned long)buff[1] << 0);
    }
    return  ((unsigned long)buff[1] << 8) | 
            ((unsigned long)buff[0] << 0);
}

unsigned long get_dword( unsigned char * buff )
{
    if( big_endian )
    {
        return ((unsigned long)buff[0] << 24) | 
               ((unsigned long)buff[1] << 16) | 
               ((unsigned long)buff[2] <<  8) | 
               ((unsigned long)buff[3] <<  0);
    }
    return  ((unsigned long)buff[3] << 24) | 
            ((unsigned long)buff[2] << 16) | 
            ((unsigned long)buff[1] <<  8) | 
            ((unsigned long)buff[0] <<  0);
}

void print_hex_byte( unsigned char *buff, int n )
{
  int  i;
  for( i = 0; i < 16; i++ )
  {
    if( i == 8 ) fprintf( outfile, " " );
    if( i < n )
      fprintf( outfile, "%02X ", *buff++ );
    else
      fprintf( outfile, "   " );
  }
}

void print_hex_word( unsigned short *buff, int n )
{
  int  i;
  for( i = 0; i < 8; i++, buff++ )
  {
    if( i < n )
      fprintf( outfile, "%04X  ", get_word( (unsigned char*)buff ));
    else
      fprintf( outfile, "      " );
  }
}

void print_hex_dword( unsigned long *buff, int n )
{
  int  i;
  for( i = 0; i < 4; i++, buff++ )
  {
    if( i < n )
      fprintf( outfile, "%08lX  ", get_dword( (unsigned char*)buff ));
    else
      fprintf( outfile, "          " );
  }
}

void print_ascii( char *pre, char *data, int len, char *post, int width )
{
    int i;
    unsigned char c;
    
    fprintf( outfile, pre );
    for( i = 0; i < width; i++ )
    {
      c = data[i];
      if( c < 0x20 || c >= 0x7f ) c = '.';
      if( i >= len ) c = ' ';
      fprintf( outfile, "%c", c );
    }
    fprintf( outfile, post );
}

int hex_dump( FILE *file, long start, long end, int hsize )
{
  int  n;
  char Line[32];

  while( start < end )
  {
    n = (end > (start + 16)) ? 16 : (int)(end - start);
    if( fread( Line, 1, n, file ) != n ) return 2;

    fprintf( outfile, "%08lX  ", start );
    switch( hsize )
    {
      case H_BYTE : print_hex_byte( (unsigned char *)Line, n ); break;
      case H_WORD : print_hex_word( (unsigned short *)Line, n/2 ); break;
      case H_DWORD: print_hex_dword( (unsigned long *)Line, n/4 ); break;
    }
    print_ascii( " |", Line, n, "|\n", 16 );
    start += n;
  }

  return 0;
}

int hex_dump_asm( 
    FILE *file, 
    unsigned long start, 
    unsigned long end, 
    int hsize, 
    int width, 
    int flags )
{
  int  i, n;
  unsigned char Line[128];

  if( width == 0 ) width = default_line_bytes_asm[hsize];
  
  fprintf( outfile, "%s label %s", array_name, storage_type_asm[hsize] );

  while( start < end )
  {
    n = (end > (start + width)) ? width : (int)(end - start);
    if( fread( Line, 1, n, file ) != n ) return 2;

    switch( hsize )
    {
      case H_BYTE :
        fprintf( outfile, "\n    db " );
        for( i = 0; i < n; i++ )
        {
            fprintf( outfile, i?",0%02Xh":"0%02Xh", Line[i] );
        }
        break;
      case H_WORD :
        fprintf( outfile, "\n    dw " );
        for( i = 0; i < n; i += 2 )
        {
            fprintf( outfile, i?",0%04Xh":"0%04Xh", get_word(Line+i) );
        }
        break;
      case H_DWORD:
        fprintf( outfile, "\n    dd " );
        for( i = 0; i < n; i += 4 )
        {
            fprintf( outfile, i?",0%08lXh":"0%08lXh", get_dword(Line+i) );
        }
        break;
    }

    if( flags & P_OFFSET ) 
    {
        fprintf( outfile, " ; %08lX", start );
        if( flags & P_TEXT ) 
            print_ascii( " |", Line, n, "|", width );
    }
    else
    {
        if( flags & P_TEXT ) 
            print_ascii( "; |", Line, n, "|", width );
    }
    start += n;
  }

  fprintf( outfile, "\n" );

  return 0;
}

int hex_dump_c( 
    FILE *file, 
    unsigned long start, 
    unsigned long end, 
    int hsize, 
    int width, 
    int flags )
{
  int  i, n;
  unsigned char Line[128];

  if( width == 0 ) width = default_line_bytes_c[hsize];

  fprintf( outfile, "unsigned %s %s[] =\n{", storage_type_c[hsize], array_name );

  while( start < end )
  {
    n = (end > (start + width)) ? width : (int)(end - start);
    if( fread( Line, 1, n, file ) != n ) return 2;

    if( flags & P_OFFSET ) 
        fprintf( outfile, "\n    /* %08lX */ ", start );
    else
        fprintf( outfile, "\n    " );

    switch( hsize )
    {
      case H_BYTE :
        for( i = 0; i < n; i++ )
        {
            start ++;
            fprintf( outfile, (start < end)?"0x%02X,":"0x%02X ", Line[i] );
        }
        break;
      case H_WORD :
        for( i = 0; i < n; i += 2 )
        {
            start += 2;
            fprintf( outfile, (start < end)?"0x%04X,":"0x%04X ", get_word(Line+i) );
        }
        break;
      case H_DWORD:
        for( i = 0; i < n; i += 4 )
        {
            start += 4;
            fprintf( outfile, (start < end)?"0x%08lX,":"0x%08lX ", get_dword(Line+i) );
        }
        break;
    }
    if( flags & P_TEXT ) 
        print_ascii( " /* ", Line, n, " */", width );
  }

  fprintf( outfile, "\n};\n" );

  return 0;
}

typedef enum { NORM = 0, CR, LF, TAB, BKSP, HEX } char_t;

int hex_dump_text( 
    FILE *file, 
    unsigned long start, 
    unsigned long end, 
    int hsize, 
    int width, 
    int flags )
{
  int  i, j = 0, n;
  char_t ct = NORM;
  unsigned long off = start;
  unsigned char c, Line[128], buff[136];

  if( width == 0 ) width = 64;

  fprintf( outfile, "unsigned char %s[ /* %d */ ] =\n{", array_name, end-start );

  while( start < end )
  {
    n = (int)(end - start);
    if( n > sizeof(Line) ) n = sizeof(Line);

    if( fread( Line, 1, n, file ) != n ) return 2;

    for( i = 0; i < n; )
    {
        c = Line[i++];
        start ++;
        if( c >= 0x20 && c < 0x7f )
        {
            if( ct == HEX ) { buff[j++] = '"'; buff[j++] = '"'; }
            if( c == '"' || c == '\\' ) buff[j++] = '\\';
            buff[j++] = c;
            ct = NORM;
        }
        else
        {
            switch( c )
            {
            case '\0': 
                buff[j++] = '\\'; 
                buff[j++] = '0'; 
                ct = NORM;
                break; 
            case '\r': 
                buff[j++] = '\\'; 
                buff[j++] = 'r'; 
                ct = CR;
                break; 
            case '\n': 
                if( ct == CR )
                {
                    buff[j-1] = 'n'; 
                }
                else
                {
                    buff[j++] = '\\'; 
                    buff[j++] = 'n'; 
                }
                ct = LF;
                break;
            case '\t': 
                buff[j++] = '\\'; 
                buff[j++] = 't'; 
                ct = NORM;
                break;
            case '\b': 
                buff[j++] = '\\'; 
                buff[j++] = 'b'; 
                ct = NORM;
                break;
            default:
                j += sprintf( (char *)&buff[j], "\\x%02x", c );
                ct = HEX;
            }
        }
        if( j >= width )
        {
            buff[j] = 0;
            if( flags & P_OFFSET ) 
                fprintf( outfile, "\n    /* %08lX */ \"%s\"", off, buff );
            else
                fprintf( outfile, "\n    \"%s\"", buff );
            j = 0;
            ct = NORM;
            off = start;
        }
    }
  }

  if( j > 0 )
  {
      buff[j] = 0;
      if( flags & P_OFFSET ) 
          fprintf( outfile, "\n    /* %08lX */ \"%s\"", off, buff );
      else
          fprintf( outfile, "\n    \"%s\"", buff );
  }
  fprintf( outfile, "\n};\n" );

  return 0;
}

int hex_dump_java( 
    FILE *file, 
    unsigned long start, 
    unsigned long end, 
    int hsize, 
    int width, 
    int flags )
{
  int  i, n;
  unsigned char Line[128];
  
  if( width == 0 ) width = default_line_bytes_java[hsize];
  
  fprintf( outfile, "    static final %s %s[] = {", storage_type_java[hsize], array_name );

  while( start < end )
  {
    n = (end > (start + width)) ? width : (int)(end - start);
    if( fread( Line, 1, n, file ) != n ) return 2;

    if( flags & P_OFFSET )
        fprintf( outfile, "\n      /* %08lX */ ", start );
    else
        fprintf( outfile, "\n      " );

    switch( hsize )
    {
      case H_BYTE :
        for( i = 0; i < n; i++ )
        {
            start ++;
            fprintf( outfile, (start < end)?"(byte)0x%02X,":"(byte)0x%02X ", Line[i] );
        }
        break;
      case H_WORD :
        for( i = 0; i < n; i += 2 )
        {
            start += 2;
            fprintf( outfile, (start < end)?"(short)0x%04X,":"(short)0x%04X ", get_word(Line+i) );
        }
        break;
      case H_DWORD:
        for( i = 0; i < n; i += 4 )
        {
            start += 4;
            fprintf( outfile, (start < end)?"(int)0x%08lX,":"(int)0x%08lX ", get_dword(Line+i) );
        }
        break;
    }

    if( flags & P_TEXT ) 
        print_ascii( " /* ", Line, n, " */", width );
  }

  fprintf( outfile, "\n    };\n" );

  return 0;
}

int print_usage( char *pname )
{
    fprintf( stderr, 
        "Binary to Hex Convertor Version 2.7. By Mehdi Sotoodeh.\n"
        "Usage:\n"
        "      %s [options] filename [start [end]]\n"
        "Options:\n"
        "      -o:outfile   Define output file name (default=stdout).\n"
        "      -b|w|d       Byte/Word/Dword format (default=byte).\n"
        "      -r           Reverse byte order (for -w or -d).\n"
        "      -p           Include file position.\n"
        "      -t           Include text as comment.\n"
        "      -l:n         Set line width to <n> bytes per line.\n"
        "      -fa:label    ASM output format.\n"
        "      -fc:label    C output format.\n"
        "      -fj:label    Java output format.\n"
        "      start        Staring offset (in hex).\n"
        "      end          End address (in hex).\n",
        pname );
    return 1;
}

int main( int argc, char *argv[] )
{
  FILE *infile;
  int  rc, off, hsize, Style, flags, i, width;
  unsigned long start, end, fsize;
  char *infilename = NULL;

  off   = 0;
  flags = 0;
  width = 0;
  hsize = H_BYTE;
  Style = STYLE_NONE;
  start = 0L;
  end   = -1;
  outfile = stdout;

  for( i = 1; i < argc; i++ )
  {
    char * p = argv[i];
    if( p[0] == '-' )
    {
      p++;
      option_loop:
      switch( *p++ )
      {
        case 'o' :
            if( *p == ':' ) p++;
            outfile = fopen( p, "wt" );
            if( outfile == NULL )
            {
                fprintf( stderr, "Error openning output file %s.\n", p );
                return 2;
            }
            break;
        case 'p' :
            flags |= P_OFFSET;
            goto option_loop;
        case 'r' :
            big_endian ^= 1; 
            goto option_loop;
        case 'f' :
            switch( *p++ )
            {
            case 'a' : Style = STYLE_ASM; break;
            case 'c' : Style = STYLE_C; break;
            case 'j' : Style = STYLE_JAVA; break;
            case 't' : Style = STYLE_TEXT; break;
            default:
                fprintf( stderr, "Unknown output format %c.\n", p[-1] );
                return 3;
            }
            if( *p == ':' ) p++;
            array_name = p;
            break;
        case 't' :
            flags |= P_TEXT;
            goto option_loop;
        case 'l' :
            if( *p == ':' ) p++;
            width = atol(p);
            if( width > 128 ) width = 128;
            break;
        case 'b' :
            hsize = H_BYTE; 
            goto option_loop;
        case 'w' :
            hsize = H_WORD; 
            goto option_loop;
        case 'd' :
            hsize = H_DWORD; 
            goto option_loop;
        case '\0':
            if( p[-2] != '-' ) break;
        default  : 
            return print_usage( argv[0] );
      } 
      continue;
    }

    switch( off++ )
    {
        case 0: 
            infilename = p; 
            break;
        case 1:
            sscanf( p, "%lx", &start );
            break;
        case 2:
            sscanf( p, "%lx", &end );
            break;
        default:
            fprintf( stderr, "\nSyntax error.\n" ); 
            return 1;
    }
  }

  if( infilename == NULL ) return print_usage( argv[0] );
  
  infile = fopen( infilename, "rb" );
  if( infile == NULL )
  {
    fprintf( stderr, "File: %s does not exist.\n", infilename );
    return 1;
  }

  fseek( infile, 0L, SEEK_END ); 
  fsize = ftell( infile );
  fseek( infile, start, SEEK_SET );
  
  if( end > fsize ) end = fsize;

  switch( Style )
  {
    case STYLE_ASM: 
        rc = hex_dump_asm( infile, start, end, hsize, width, flags );
        break;
    case STYLE_C: 
        rc = hex_dump_c( infile, start, end, hsize, width, flags );
        break;
    case STYLE_TEXT: 
        rc = hex_dump_text( infile, start, end, hsize, width, flags );
        break;
    case STYLE_JAVA: 
        rc = hex_dump_java( infile, start, end, hsize, width, flags );
        break;
    default:
        rc = hex_dump( infile, start, end, hsize );
        break;
  }
  
  if( rc == 2 ) fprintf( stderr, "\nError reading '%s'.", infilename );
  
  fclose( infile);
  if( outfile != stdout ) fclose( outfile );
  return rc;
}