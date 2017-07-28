/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Developed by Jacob Navia                                           */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

//-----------------------------------------------------------------------------
// DIB.C
//
// This is a collection of useful DIB manipulation/information gathering
// functions.  Many functions are supplied simply to take the burden
// of taking into account whether a DIB is a Win30 style or OS/2 style
// DIB away from the application.
//
// The functions in this module assume that the DIB pointers or handles
// passed to them point to a block of memory in one of two formats:
//
//       a) BITMAPINFOHEADER + color table + DIB bits (3.0 style DIB)
//       b) BITMAPCOREHEADER + color table + DIB bits (OS/2 PM style)
//
// The SDK Reference, Volume 2 describes these data structures.
//
// A number of functions in this module were lifted from SHOWDIB,
// and modified to handle OS/2 DIBs.
//
// The functions in this module could be streamlined (made faster and
// smaller) by removing the OS/2 DIB specific code, and assuming all
// DIBs passed to it are Win30 style DIBs.  The DIB file reading code
// would need to be modified to always convert DIBs to Win30 style
// DIBs.  The only reason this isn't done in DIBView is because DIBView
// was written to test display and printer drivers (which are supposed
// to support OS/2 DIBs wherever they support Win30 style DIBs).  SHOWDIB
// is a great example of how to go about doing this.
//-----------------------------------------------------------------------------


#include <windows.h>
#include <caml/memory.h>
#include <string.h>
#include <caml/io.h>
#include <stdio.h>
   // Size of window extra bytes (we store a handle to a PALINFO structure).

#define PAL_CBWNDEXTRA  (1 * sizeof (WORD))


typedef struct
   {
   HPALETTE hPal;                      // Handle to palette being displayed.
   WORD     wEntries;                  // # of entries in the palette.
   int      nSquareSize;               // Size of palette square (see PAL_SIZE)
   HWND     hInfoWnd;                  // Handle to the info bar window.
   int      nRows, nCols;              // # of Rows/Columns in window.
   int      cxSquare, cySquare;        // Pixel width/height of palette square.
   WORD     wEntry;                    // Currently selected palette square.
   } PALINFO, FAR *LPPALINFO;
   // Window Words.
#define WW_PAL_HPALINFO 0              // Handle to PALINFO structure.
   // The following define is for CopyPaletteChangingFlags().
#define DONT_CHANGE_FLAGS -1
   // The following is the palette version that goes in a
   //  LOGPALETTE's palVersion field.
#define PALVERSION   0x300
// This is an enumeration for the various ways we can display
//  a palette in PaletteWndProc().
enum PAL_SIZE
   {
   PALSIZE_TINY = 0,
   PALSIZE_SMALL,
   PALSIZE_MEDIUM,
   PALSIZE_LARGE
   };
#define CopyPalette(hPal)         CopyPaletteChangingFlags (hPal, DONT_CHANGE_FLAGS)
#define CopyPalForAnimation(hPal) CopyPaletteChangingFlags (hPal, PC_RESERVED)
// WIDTHBYTES takes # of bits in a scan line and rounds up to nearest
//  word.
#define WIDTHBYTES(bits)      (((bits) + 31) / 32 * 4)

   // Given a pointer to a DIB header, return TRUE if is a Windows 3.0 style
   //  DIB, false if otherwise (PM style DIB).
#define IS_WIN30_DIB(lpbi)  ((*(LPDWORD) (lpbi)) == sizeof (BITMAPINFOHEADER))

static WORD     PaletteSize          (LPSTR lpbi);

extern void ShowDbgMsg(char *);
static BOOL     MyRead            (int, LPSTR, DWORD);
/*--------------  DIB header Marker Define -------------------------*/
#define DIB_HEADER_MARKER   ((WORD) ('M' << 8) | 'B')
/*--------------  MyRead Function Define ---------------------------*/

// When we read in a DIB, we read it in in chunks.  We read half a segment
//  at a time.  This way we insure that we don't cross any segment
//  boundries in _lread() during a read.  We don't read in a full segment
//  at a time, since _lread takes some "int" type parms instead of
//  WORD type params (it'd work, but the compiler would give you warnings)...

#define BYTES_PER_READ  32767

/*--------------  Define for PM DIB  -------------------------------*/
// The constants for RGB, RLE4, RLE8 are already defined inside
// of Windows.h

#define BI_PM       3L


/*-------------- Magic numbers -------------------------------------*/
// Maximum length of a filename for DOS is 128 characters.

#define MAX_FILENAME 129


/*--------------  TypeDef Structures -------------------------------*/

typedef struct InfoStruct
  {
  char  szName[13];
  char  szType[15];
  DWORD cbWidth;
  DWORD cbHeight;
  DWORD cbColors;
  char  szCompress[5];
  }  INFOSTRUCT;

// Some macros.
#define RECTWIDTH(lpRect)     ((lpRect)->right - (lpRect)->left)
#define RECTHEIGHT(lpRect)    ((lpRect)->bottom - (lpRect)->top)
//---------------------------------------------------------------------
//
// Function:   FindDIBBits
//
// Purpose:    Given a pointer to a DIB, returns a pointer to the
//             DIB's bitmap bits.
//
// Parms:      lpbi == pointer to DIB header (either BITMAPINFOHEADER
//                       or BITMAPCOREHEADER)
//
// History:   Date      Reason
//             6/01/91  Created
//
//---------------------------------------------------------------------
static LPSTR FindDIBBits (LPSTR lpbi)
{
   return (lpbi + *(LPDWORD)lpbi + PaletteSize (lpbi));
}


//---------------------------------------------------------------------
//
// Function:   DIBNumColors
//
// Purpose:    Given a pointer to a DIB, returns a number of colors in
//             the DIB's color table.
//
// Parms:      lpbi == pointer to DIB header (either BITMAPINFOHEADER
//                       or BITMAPCOREHEADER)
//
// History:   Date      Reason
//             6/01/91  Created
//
//---------------------------------------------------------------------
static WORD DIBNumColors (LPSTR lpbi)
{
   WORD wBitCount;


      // If this is a Windows style DIB, the number of colors in the
      //  color table can be less than the number of bits per pixel
      //  allows for (i.e. lpbi->biClrUsed can be set to some value).
      //  If this is the case, return the appropriate value.

   if (IS_WIN30_DIB (lpbi))
      {
      DWORD dwClrUsed;

      dwClrUsed = ((LPBITMAPINFOHEADER) lpbi)->biClrUsed;

      if (dwClrUsed)
         return (WORD) dwClrUsed;
      }


      // Calculate the number of colors in the color table based on
      //  the number of bits per pixel for the DIB.

   if (IS_WIN30_DIB (lpbi))
      wBitCount = ((LPBITMAPINFOHEADER) lpbi)->biBitCount;
   else
      wBitCount = ((LPBITMAPCOREHEADER) lpbi)->bcBitCount;

   switch (wBitCount)
      {
      case 1:
         return 2;

      case 4:
         return 16;

      case 8:
         return 256;

      default:
         return 0;
      }
}

//---------------------------------------------------------------------
//
// Function:   PaletteSize
//
// Purpose:    Given a pointer to a DIB, returns number of bytes
//             in the DIB's color table.
//
// Parms:      lpbi == pointer to DIB header (either BITMAPINFOHEADER
//                       or BITMAPCOREHEADER)
//
// History:   Date      Reason
//             6/01/91  Created
//
//---------------------------------------------------------------------
static WORD PaletteSize (LPSTR lpbi)
{
   if (IS_WIN30_DIB (lpbi))
      return (DIBNumColors (lpbi) * sizeof (RGBQUAD));
   else
      return (DIBNumColors (lpbi) * sizeof (RGBTRIPLE));
}

//---------------------------------------------------------------------
//
// Function:   DIBHeight
//
// Purpose:    Given a pointer to a DIB, returns its height.  Note
//             that it returns a DWORD (since a Win30 DIB can have
//             a DWORD in its height field), but under Win30, the
//             high order word isn't used!
//
// Parms:      lpDIB == pointer to DIB header (either BITMAPINFOHEADER
//                       or BITMAPCOREHEADER)
//
// History:   Date      Reason
//             6/01/91  Created
//
//---------------------------------------------------------------------
static DWORD DIBHeight (LPSTR lpDIB)
{
   LPBITMAPINFOHEADER lpbmi;
   LPBITMAPCOREHEADER lpbmc;

   lpbmi = (LPBITMAPINFOHEADER) lpDIB;
   lpbmc = (LPBITMAPCOREHEADER) lpDIB;

   if (lpbmi->biSize == sizeof (BITMAPINFOHEADER))
      return lpbmi->biHeight;
   else
      return (DWORD) lpbmc->bcHeight;
}

/*************************************************************************

  Function:  ReadDIBFile (int)

   Purpose:  Reads in the specified DIB file into a global chunk of
             memory.

   Returns:  A handle to a dib (hDIB) if successful.
             NULL if an error occurs.

  Comments:  BITMAPFILEHEADER is stripped off of the DIB.  Everything
             from the end of the BITMAPFILEHEADER structure on is
             returned in the global memory handle.

   History:   Date      Author      Reason

             6/1/91    Created
             6/27/91   Removed PM bitmap conversion routines.
             6/31/91   Removed logic which overallocated memory
                       (to account for bad display drivers).
            11/08/91   Again removed logic which overallocated
                       memory (it had creeped back in!)

*************************************************************************/
static HANDLE ReadDIBFile (int hFile,int dwBitsSize)
{
   BITMAPFILEHEADER   bmfHeader;
   HANDLE             hDIB;
   LPSTR              pDIB;



   // Go read the DIB file header and check if it's valid.

   if ((_lread (hFile, (LPSTR) &bmfHeader, sizeof (bmfHeader)) != sizeof (bmfHeader)) ||
        (bmfHeader.bfType != DIB_HEADER_MARKER))
      {
        //              ShowDbgMsg("Not a DIB file!");
                return NULL;
      }

   // Allocate memory for DIB

   hDIB = GlobalAlloc (GMEM_SHARE|GMEM_MOVEABLE | GMEM_ZEROINIT, dwBitsSize - sizeof(BITMAPFILEHEADER));

   if (hDIB == 0)
     {
       //       ShowDbgMsg("Couldn't allocate memory!");
                return NULL;
     }

   pDIB = GlobalLock (hDIB);

   // Go read the bits.

   if (!MyRead (hFile, pDIB, dwBitsSize - sizeof(BITMAPFILEHEADER)))
      {
      GlobalUnlock (hDIB);
      GlobalFree   (hDIB);
      //  ShowDbgMsg("Error reading file!");
      return NULL;
      }


   GlobalUnlock (hDIB);
   return hDIB;
}

/*************************************************************************

  Function:  MyRead (int, LPSTR, DWORD)

   Purpose:  Routine to read files greater than 64K in size.

   Returns:  TRUE if successful.
             FALSE if an error occurs.

  Comments:

   History:   Date     Reason

             6/1/91    Created

*************************************************************************/
static BOOL MyRead (int hFile, LPSTR lpBuffer, DWORD dwSize)
{
   char *lpInBuf = (char *) lpBuffer;
   int       nBytes;


   while (dwSize)
      {
      nBytes = (int) (dwSize > (DWORD) BYTES_PER_READ ? BYTES_PER_READ :
                                                        LOWORD (dwSize));

      if (_lread (hFile, (LPSTR) lpInBuf, nBytes) != (WORD) nBytes)
         return FALSE;

      dwSize  -= nBytes;
      lpInBuf += nBytes;
      }

   return TRUE;
}

//---------------------------------------------------------------------
//
// Function:   DIBPaint
//
// Purpose:    Painting routine for a DIB.  Calls StretchDIBits() or
//             SetDIBitsToDevice() to paint the DIB.  The DIB is
//             output to the specified DC, at the coordinates given
//             in lpDCRect.  The area of the DIB to be output is
//             given by lpDIBRect.  The specified palette is used.
//
// Parms:      hDC       == DC to do output to.
//             lpDCRect  == Rectangle on DC to do output to.
//             hDIB      == Handle to global memory with a DIB spec
//                          in it (either a BITMAPINFO or BITMAPCOREINFO
//                          followed by the DIB bits).
//             lpDIBRect == Rect of DIB to output into lpDCRect.
//             hPal      == Palette to be used.
//
// History:   Date      Reason
//             6/01/91  Created
//
//---------------------------------------------------------------------
static void DIBPaint (HDC hDC,LPRECT lpDCRect,HANDLE hDIB)
{
   LPSTR    lpDIBHdr, lpDIBBits;

   if (!hDIB)
      return;
      // Lock down the DIB, and get a pointer to the beginning of the bit
      //  buffer.
        lpDIBHdr  = GlobalLock (hDIB);
        lpDIBBits = FindDIBBits (lpDIBHdr);
      // Make sure to use the stretching mode best for color pictures.
        SetStretchBltMode (hDC, COLORONCOLOR);
        SetDIBitsToDevice (hDC,                          // hDC
                         lpDCRect->left,               // DestX
                         lpDCRect->top,                // DestY
                         RECTWIDTH (lpDCRect),         // nDestWidth
                         RECTHEIGHT (lpDCRect),        // nDestHeight
                                                                 0,              // SrcX
                         0,
 //                        (int) DIBHeight (lpDIBHdr),   // SrcY
                                                                 0,                            // nStartScan
                         (WORD) DIBHeight (lpDIBHdr),  // nNumScans
                         lpDIBBits,                    // lpBits
                         (LPBITMAPINFO) lpDIBHdr,      // lpBitsInfo
                         DIB_RGB_COLORS);              // wUsage

   GlobalUnlock (hDIB);
}

static unsigned int Getfilesize(char *name)
{
        FILE *f;
        unsigned int size;

        f = fopen(name,"rb");
        if (f == NULL)
                return 0;
        fseek(f,0,SEEK_END);
        size = ftell(f);
        fclose(f);
        return size;
}


HANDLE ChargerBitmap(char *FileName,POINT *lppt)
{
        HFILE hFile;
        OFSTRUCT ofstruct;
        HANDLE result;
        LPSTR    lpDIBHdr;
        unsigned int size;

        size = Getfilesize(FileName);
        hFile=OpenFile((LPSTR) FileName, &ofstruct, OF_READ | OF_SHARE_DENY_WRITE);
        result =  ReadDIBFile(hFile,size);
        if (hFile) _lclose(hFile);
        if (result) {
                LPBITMAPINFOHEADER lpbmi;
                LPBITMAPCOREHEADER lpbmc;

                lpDIBHdr  = GlobalLock (result);
                lpbmi = (LPBITMAPINFOHEADER) lpDIBHdr;
                lpbmc = (LPBITMAPCOREHEADER) lpDIBHdr;

                if (lpbmi->biSize == sizeof (BITMAPINFOHEADER)) {
                        lppt->y = lpbmi->biHeight;
                        lppt->x = lpbmi->biWidth;
                }
                else {
                        lppt->y = lpbmc->bcHeight;
                        lppt->x = lpbmc->bcWidth;
                }
                GlobalUnlock(result);
        }
        return(result);
}

void DessinerBitmap(HANDLE hDIB,HDC hDC,LPRECT lpDCRect)
{
        DIBPaint (hDC,
             lpDCRect,
             hDIB);
}

void AfficheBitmap(char *filename,HDC hDC,int x,int y)
{
        RECT rc;
        HANDLE hdib;
        POINT pt;
        char titi[60];

        hdib = ChargerBitmap(filename,&pt);
        if (hdib == NULL) {
                return;
    }
        rc.top = y;
        rc.left = x;
        rc.right = pt.x+x;
        rc.bottom = pt.y+y;
        pt.y += GetSystemMetrics(SM_CYCAPTION);
        DessinerBitmap(hdib,hDC,&rc);
        GlobalFree(hdib);
}
