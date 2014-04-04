#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned long u32;

struct dmi_header
{
    u8  type;
    u8  length;
    u16 handle;
};

u16 get_LSBF16( u8 * data )
{
    return (u16)((((u16)data[1]) << 8) | data[0]);
}

u32 get_LSBF32( u8 * data )
{
    return (u32)((((u32)data[3]) << 24) | 
           (((u32)data[2]) << 16) |
           (((u32)data[1]) << 8) | data[0]);
}

static char *dmi_string(struct dmi_header *dm, u8 s)
{
    char *bp=(char *)dm;
    if (!s) return "";
    
    bp+=dm->length;
    while (s>1) {
        bp+=strlen(bp);
        bp++;
        s--;
    }
    return bp;
}

static void dmi_table(u32 base, int len, int num)
{
    char *buf = (char *)((base & ~0xf) << 12);
    struct dmi_header *dm;
    char *data;
    int i=0;
    
    buf += (base & 0xf);
    
    data = buf;
    while (i<num) {
        dm=(struct dmi_header *)data;
        switch(dm->type)
        {
        case  0:
            printf("\tBIOS Information (type %d, len %d, handle %04X)\n",
                dm->type, dm->length, dm->handle);
            printf("\t\tVendor: %s\n", 
                dmi_string(dm, data[4]));
            printf("\t\tVersion: %s\n", dmi_string(dm, data[5]));
            printf("\t\tRelease: %s\n", dmi_string(dm, data[8]));
            printf("\t\tBIOS base: 0x%04X0\n", get_LSBF16( data + 6 ));
            printf("\t\tROM size: %ldK\n", (u32)(64*(u32)data[9]));
            printf("\t\tCapabilities:\n");
            printf("\t\t\tFlags: 0x%08X%08X\n",
               get_LSBF32( data + 14 ), get_LSBF32( data + 10 ));
            break;
            
        case 1:
            printf("\tSystem Information Block(type %d, len %d, handle %04X)\n",
                dm->type, dm->length, dm->handle);
            printf("\t\tVendor: %s\n", dmi_string(dm, data[4]));
            printf("\t\tProduct: %s\n", dmi_string(dm, data[5]));
            printf("\t\tVersion: %s\n", dmi_string(dm, data[6]));
            printf("\t\tSerial Number: %s\n", dmi_string(dm, data[7]));
            break;
            
        case 2:
            printf("\tBoard Information Block(type %d, len %d, handle %04X)\n",
                dm->type, dm->length, dm->handle);
            printf("\t\tVendor: %s\n", dmi_string(dm, data[4]));
            printf("\t\tProduct: %s\n", dmi_string(dm, data[5]));
            printf("\t\tVersion: %s\n", dmi_string(dm, data[6]));
            printf("\t\tSerial Number: %s\n", dmi_string(dm, data[7]));
            break;
            
        default:
            printf("\tStructure (type %d, len %d, handle %04X)\n",
                dm->type, dm->length, dm->handle);
            {
                u8 s = 1, *p;
                while( 1 )
                {
                    p = dmi_string(dm, s);
                    if( !p[0] ) break;
                    printf("\t\tString %d: %s\n", s, p);
                    s++;
                }
            }
            break;
        }
        data+=dm->length;
        while(data[0] || data[1]) data++;
        data+=2;
        i++;
    }
}

int main( int argc, char *argv[] )
{
    unsigned short fp;
    
    for( fp = 0; fp < 0xFFF0; fp += 16 ) 
    {
        u8 *cur  = ((u8 far *)0xf0000000) + fp;
        if( memcmp( cur, "_SM_", 4 ) == 0 ) 
        {        
            printf( "SMBIOS %d.%d rev %d present at 0x%04X.\n", 
                cur[6], cur[7], cur[10], fp );
            printf( "Max structure size of 0x%04X bytes.\n", get_LSBF16( cur + 8 ));
        }
    }

    for( fp = 0; fp < 0xFFF0; fp += 16 ) 
    {
        u8 *cur  = ((u8 far *)0xf0000000) + fp;
        if(memcmp( cur, "_DMI_", 5 ) == 0 ) 
        {
            u16 num  = get_LSBF16( cur + 12 );
            u16 len  = get_LSBF16( cur + 6 );
            u32 base = get_LSBF32( cur + 8 );

            printf( "\nDMI %d.%d present at 0x%04X.\n", 
                cur[14] >> 4, cur[14] & 0x0F, fp );
            printf( "%d structures occupying %d bytes.\n", num, len );
            printf( "DMI table at 0x%08lX.\n", base );
            dmi_table( base, len, num );
        }
    }
}
