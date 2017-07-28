/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*  Developed by Jacob Navia, based on code by J-M Geffroy and X Leroy */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#include <math.h>
#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/fail.h"
#include "libgraph.h"
#include "caml/custom.h"
#include "caml/memory.h"

HDC gcMetaFile;
int grdisplay_mode;
int grremember_mode;
GR_WINDOW grwindow;

static void GetCurrentPosition(HDC hDC,POINT *pt)
{
        MoveToEx(hDC,0,0,pt);
        MoveToEx(hDC,pt->x,pt->y,0);
}

static value gr_draw_or_fill_arc(value vx, value vy, value vrx, value vry,
                                 value vstart, value vend, BOOL fill);

CAMLprim value caml_gr_plot(value vx, value vy)
{
        int x = Int_val(vx);
        int y = Int_val(vy);
        gr_check_open();
        if(grremember_mode)
                SetPixel(grwindow.gcBitmap, x, Wcvt(y),grwindow.CurrentColor);
        if(grdisplay_mode) {
                SetPixel(grwindow.gc, x, Wcvt(y),grwindow.CurrentColor);
        }
        return Val_unit;
}

CAMLprim value caml_gr_moveto(value vx, value vy)
{
        grwindow.grx = Int_val(vx);
        grwindow.gry = Int_val(vy);
        if(grremember_mode)
                MoveToEx(grwindow.gcBitmap,grwindow.grx,Wcvt(grwindow.gry),0);
        if (grdisplay_mode)
                MoveToEx(grwindow.gc,grwindow.grx,Wcvt(grwindow.gry),0);
        return Val_unit;
}

CAMLprim value caml_gr_current_x(value unit)
{
        return Val_int(grwindow.grx);
}

CAMLprim value caml_gr_current_y(value unit)
{
        return Val_int(grwindow.gry);
}

CAMLprim value caml_gr_lineto(value vx, value vy)
{
        int x = Int_val(vx);
        int y = Int_val(vy);
        gr_check_open();
        SelectObject(grwindow.gc,grwindow.CurrentPen);
        SelectObject(grwindow.gcBitmap,grwindow.CurrentPen);
        if (grremember_mode)
                LineTo(grwindow.gcBitmap,x,Wcvt(y));
        if (grdisplay_mode)
                LineTo(grwindow.gc, x, Wcvt(y));
        grwindow.grx = x;
        grwindow.gry = y;
        return Val_unit;
}

CAMLprim value caml_gr_draw_rect(value vx, value vy, value vw, value vh)
{
        int     x, y, w, h;
        POINT pt[5];
        x=Int_val(vx);
        y=Wcvt(Int_val(vy));
        w=Int_val(vw);
        h=Int_val(vh);

        pt[0].x = x;         pt[0].y = y - h;
        pt[1].x = x + w;     pt[1].y = y - h;
        pt[2].x = x + w;     pt[2].y = y;
        pt[3].x = x;         pt[3].y = y;
        pt[4].x = x;         pt[4].y = y - h;
        if (grremember_mode) {
                Polyline(grwindow.gcBitmap,pt, 5);
        }
        if (grdisplay_mode) {
                Polyline(grwindow.gc,pt, 5);
        }
        return Val_unit;
}

CAMLprim value caml_gr_draw_text(value text,value x)
{
        POINT pt;
        int oldmode = SetBkMode(grwindow.gc,TRANSPARENT);
        SetBkMode(grwindow.gcBitmap,TRANSPARENT);
        SetTextAlign(grwindow.gcBitmap, TA_UPDATECP|TA_BOTTOM);
        SetTextAlign(grwindow.gc, TA_UPDATECP|TA_BOTTOM);
        if (grremember_mode) {
                TextOut(grwindow.gcBitmap,0,0,(char *)text,x);
        }
        if(grdisplay_mode) {
                TextOut(grwindow.gc,0,0,(char *)text,x);
        }
        GetCurrentPosition(grwindow.gc,&pt);
        grwindow.grx = pt.x;
        grwindow.gry = grwindow.height - pt.y;
        SetBkMode(grwindow.gc,oldmode);
        SetBkMode(grwindow.gcBitmap,oldmode);
        return Val_unit;
}

CAMLprim value caml_gr_fill_rect(value vx, value vy, value vw, value vh)
{
        int x = Int_val(vx);
        int y = Int_val(vy);
        int w = Int_val(vw);
        int h = Int_val(vh);
        RECT rc;

        gr_check_open();
        rc.left = x;
        rc.top = Wcvt(y);
        rc.right = x+w;
        rc.bottom = Wcvt(y)-h;
        if (grdisplay_mode)
                FillRect(grwindow.gc,&rc,grwindow.CurrentBrush);
        if (grremember_mode)
                FillRect(grwindow.gcBitmap,&rc,grwindow.CurrentBrush);
        return Val_unit;
}

CAMLprim value caml_gr_sound(value freq, value vdur)
{
        Beep(freq,vdur);
        return Val_unit;
}

CAMLprim value caml_gr_point_color(value vx, value vy)
{
        int x = Int_val(vx);
        int y = Int_val(vy);
        COLORREF rgb;
        unsigned long b,g,r;

        gr_check_open();
        rgb = GetPixel(grwindow.gcBitmap,x,Wcvt(y));
        b = (unsigned long)((rgb & 0xFF0000) >> 16);
        g = (unsigned long)((rgb & 0x00FF00) >> 8);
        r = (unsigned long)(rgb & 0x0000FF);
        return Val_long((r<<16) + (g<<8) + b);
}

CAMLprim value caml_gr_circle(value x,value y,value radius)
{
        int left,top,right,bottom;

        gr_check_open();
        left = x - radius/2;
        top = Wcvt(y) - radius/2;
        right = left+radius;
        bottom = top+radius;
        Ellipse(grwindow.gcBitmap,left,top,right,bottom);
        return Val_unit;
}

CAMLprim value caml_gr_set_window_title(value text)
{
        SetWindowText(grwindow.hwnd,(char *)text);
        return Val_unit;
}

CAMLprim value caml_gr_draw_arc(value *argv, int argc)
{
  return gr_draw_or_fill_arc(argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5], FALSE);
}

CAMLprim value caml_gr_draw_arc_nat(vx, vy, vrx, vry, vstart, vend)
{
  return gr_draw_or_fill_arc(vx, vy, vrx, vry, vstart, vend, FALSE);
}

CAMLprim value caml_gr_set_line_width(value vwidth)
{
        int width = Int_val(vwidth);
        HPEN oldPen,newPen;

        gr_check_open();
        oldPen = grwindow.CurrentPen;
        newPen = CreatePen(PS_SOLID,width,grwindow.CurrentColor);
        SelectObject(grwindow.gcBitmap,newPen);
        SelectObject(grwindow.gc,newPen);
        DeleteObject(oldPen);
        grwindow.CurrentPen = newPen;
        return Val_unit;
}

CAMLprim value caml_gr_set_color(value vcolor)
{
        HBRUSH oldBrush, newBrush;
        LOGBRUSH lb;
        LOGPEN pen;
        HPEN newPen;
        int color = Long_val(vcolor);

        int  r = (color & 0xFF0000) >> 16,
        g = (color & 0x00FF00) >> 8 ,
        b =  color & 0x0000FF;
        COLORREF c = RGB(r,g,b);
        memset(&lb,0,sizeof(lb));
        memset(&pen,0,sizeof(LOGPEN));
        gr_check_open();
        GetObject(grwindow.CurrentPen,sizeof(LOGPEN),&pen);
        pen.lopnColor = c;
        newPen = CreatePenIndirect(&pen);
        SelectObject(grwindow.gcBitmap,newPen);
        SelectObject(grwindow.gc,newPen);
        DeleteObject(grwindow.CurrentPen);
        grwindow.CurrentPen = newPen;
        SetTextColor(grwindow.gc,c);
        SetTextColor(grwindow.gcBitmap,c);
        oldBrush = grwindow.CurrentBrush;
        lb.lbStyle = BS_SOLID;
        lb.lbColor = c;
        newBrush = CreateBrushIndirect(&lb);
        SelectObject(grwindow.gc,newBrush);
        SelectObject(grwindow.gcBitmap,newBrush);
        DeleteObject(oldBrush);
        grwindow.CurrentBrush = newBrush;
        grwindow.CurrentColor = c;
        return Val_unit;
}


static value gr_draw_or_fill_arc(value vx, value vy, value vrx, value vry,
                                 value vstart, value vend, BOOL fill)
{
        int x, y, r_x, r_y, start, end;
        int     x1, y1, x2, y2, x3, y3, x4, y4;
        double cvt = 3.141592653/180.0;

        r_x = Int_val(vrx);
        r_y = Int_val(vry);
        if ((r_x < 0) || (r_y < 0))
                invalid_argument("draw_arc: radius must be positive");
        x     = Int_val(vx);
        y     = Int_val(vy);
        start = Int_val(vstart);
        end   = Int_val(vend);

        // Upper-left corner of bounding rect.
        x1=     x - r_x;
        y1=     y + r_y;
        // Lower-right corner of bounding rect.
        x2=     x + r_x;
        y2=     y - r_y;
        // Starting point
        x3=x + (int)(100.0*cos(cvt*start));
        y3=y + (int)(100.0*sin(cvt*start));
        // Ending point
        x4=x + (int)(100.0*cos(cvt*end));
        y4=y + (int)(100.0*sin(cvt*end));

        if (grremember_mode) {
                SelectObject(grwindow.gcBitmap,grwindow.CurrentPen);
                SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
                if( fill )
                        Pie(grwindow.gcBitmap,x1, Wcvt(y1), x2, Wcvt(y2),
                                x3, Wcvt(y3), x4, Wcvt(y4));
                else
                        Arc(grwindow.gcBitmap,x1, Wcvt(y1), x2, Wcvt(y2),
                                x3, Wcvt(y3), x4, Wcvt(y4));
        }
        if( grdisplay_mode ) {
                SelectObject(grwindow.gc,grwindow.CurrentPen);
                SelectObject(grwindow.gc,grwindow.CurrentBrush);
                if (fill)
                        Pie(grwindow.gc,x1, Wcvt(y1), x2, Wcvt(y2),
                                x3, Wcvt(y3), x4, Wcvt(y4));
                else
                        Arc(grwindow.gc,x1, Wcvt(y1), x2, Wcvt(y2),
                                x3, Wcvt(y3), x4, Wcvt(y4));
        }
        return Val_unit;
}

CAMLprim value caml_gr_show_bitmap(value filename,int x,int y)
{
        AfficheBitmap(filename,grwindow.gcBitmap,x,Wcvt(y));
        AfficheBitmap(filename,grwindow.gc,x,Wcvt(y));
        return Val_unit;
}



CAMLprim value caml_gr_get_mousex(value unit)
{
        POINT pt;
        GetCursorPos(&pt);
        MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
        return pt.x;
}

CAMLprim value caml_gr_get_mousey(value unit)
{
        POINT pt;
        GetCursorPos(&pt);
        MapWindowPoints(HWND_DESKTOP,grwindow.hwnd,&pt,1);
        return grwindow.height - pt.y - 1;
}


static void gr_font(char *fontname)
{
        HFONT hf = CreationFont(fontname);

        if (hf && hf != INVALID_HANDLE_VALUE) {
                HFONT oldFont = SelectObject(grwindow.gc,hf);
                SelectObject(grwindow.gcBitmap,hf);
                DeleteObject(grwindow.CurrentFont);
                grwindow.CurrentFont = hf;
        }
}

CAMLprim value caml_gr_set_font(value fontname)
{
        gr_check_open();
        gr_font(String_val(fontname));
        return Val_unit;
}

CAMLprim value caml_gr_set_text_size (value sz)
{
        return Val_unit;
}

CAMLprim value caml_gr_draw_char(value chr)
{
        char str[1];
        gr_check_open();
        str[0] = Int_val(chr);
        caml_gr_draw_text((value)str, 1);
        return Val_unit;
}

CAMLprim value caml_gr_draw_string(value str)
{
        gr_check_open();
        caml_gr_draw_text(str, string_length(str));
        return Val_unit;
}

CAMLprim value caml_gr_text_size(value str)
{
        SIZE extent;
        value res;

        mlsize_t len = string_length(str);
        if (len > 32767) len = 32767;

        GetTextExtentPoint(grwindow.gc,String_val(str), len,&extent);

        res = alloc_tuple(2);
        Field(res, 0) = Val_long(extent.cx);
        Field(res, 1) = Val_long(extent.cy);

        return res;
}

CAMLprim value caml_gr_fill_poly(value vect)
{
        int n_points, i;
        POINT   *p,*poly;
        n_points = Wosize_val(vect);
        if (n_points < 3)
                gr_fail("fill_poly: not enough points",0);

        poly = (POINT *)malloc(n_points*sizeof(POINT));

        p = poly;
        for( i = 0; i < n_points; i++ ){
                p->x = Int_val(Field(Field(vect,i),0));
                p->y = Wcvt(Int_val(Field(Field(vect,i),1)));
                p++;
        }
        if (grremember_mode) {
                SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
                Polygon(grwindow.gcBitmap,poly,n_points);
        }
        if (grdisplay_mode) {
                SelectObject(grwindow.gcBitmap,grwindow.CurrentBrush);
                Polygon(grwindow.gc,poly,n_points);
        }
        free(poly);

        return Val_unit;
}

CAMLprim value caml_gr_fill_arc(value *argv, int argc)
{
  return gr_draw_or_fill_arc(argv[0], argv[1], argv[2], argv[3],
                             argv[4], argv[5], TRUE);
}

CAMLprim value caml_gr_fill_arc_nat(vx, vy, vrx, vry, vstart, vend)
{
  return gr_draw_or_fill_arc(vx, vy, vrx, vry, vstart, vend, TRUE);
}

// Image primitives
struct image {
        int w;
        int h;
        HBITMAP data;
        HBITMAP mask;
};

#define Width(i) (((struct image *)Data_custom_val(i))->w)
#define Height(i) (((struct image *)Data_custom_val(i))->h)
#define Data(i) (((struct image *)Data_custom_val(i))->data)
#define Mask(i) (((struct image *)Data_custom_val(i))->mask)
#define Max_image_mem 500000

static void finalize_image (value i)
{
        DeleteObject (Data(i));
        if (Mask(i) != NULL) DeleteObject(Mask(i));
}

static struct custom_operations image_ops = {
        "_image",
        finalize_image,
        custom_compare_default,
        custom_hash_default,
        custom_serialize_default,
        custom_deserialize_default,
        custom_compare_ext_default
};

CAMLprim value caml_gr_create_image(value vw, value vh)
{
        HBITMAP cbm;
        value res;
        int w = Int_val(vw);
        int h = Int_val(vh);

        if (w < 0 || h < 0)
                gr_fail("create_image: width and height must be positive",0);

        cbm = CreateCompatibleBitmap(grwindow.gc, w, h);
        if (cbm == NULL)
                gr_fail("create_image: cannot create bitmap", 0);
        res = alloc_custom(&image_ops, sizeof(struct image),
                w * h, Max_image_mem);
        if (res) {
                Width (res) = w;
                Height (res) = h;
                Data (res) = cbm;
                Mask (res) = NULL;
        }
        return res;
}

CAMLprim value caml_gr_blit_image (value i, value x, value y)
{
        HBITMAP oldBmp = SelectObject(grwindow.tempDC,Data(i));
        int xsrc = Int_val(x);
        int ysrc = Wcvt(Int_val(y) + Height(i) - 1);
        BitBlt(grwindow.tempDC,0, 0, Width(i), Height(i),
                grwindow.gcBitmap, xsrc, ysrc, SRCCOPY);
        SelectObject(grwindow.tempDC,oldBmp);
        return Val_unit;
}


CAMLprim value caml_gr_draw_image(value i, value x, value y)
{
        HBITMAP oldBmp;

        int xdst = Int_val(x);
        int ydst = Wcvt(Int_val(y)+Height(i)-1);
        if (Mask(i) == NULL) {
                if (grremember_mode) {
                        oldBmp = SelectObject(grwindow.tempDC,Data(i));
                        BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCCOPY);
                        SelectObject(grwindow.tempDC,oldBmp);
                }
                if (grdisplay_mode) {
                        oldBmp = SelectObject(grwindow.tempDC,Data(i));
                        BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCCOPY);
                        SelectObject(grwindow.tempDC,oldBmp);
                }
        }
        else {
                if (grremember_mode) {
                        oldBmp = SelectObject(grwindow.tempDC,Mask(i));
                        BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCAND);
                        SelectObject(grwindow.tempDC,Data(i));
                        BitBlt(grwindow.gcBitmap,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCPAINT);
                        SelectObject(grwindow.tempDC,oldBmp);
                }
                if (grdisplay_mode) {
                        oldBmp = SelectObject(grwindow.tempDC,Mask(i));
                        BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCAND);
                        SelectObject(grwindow.tempDC,Data(i));
                        BitBlt(grwindow.gc,xdst, ydst, Width(i), Height(i),
                                grwindow.tempDC, 0, 0, SRCPAINT);
                        SelectObject(grwindow.tempDC,oldBmp);
                }
        }

        return Val_unit;
}

CAMLprim value caml_gr_make_image(value matrix)
{
        int width, height,has_transp,i,j;
        value img;
        HBITMAP oldBmp;
        height = Wosize_val(matrix);
        if (height == 0) {
                width = 0;
        }
        else {
                width = Wosize_val(Field(matrix, 0));
                for (i = 1; i < height; i++) {
                        if (width != (int) Wosize_val(Field(matrix, i)))
                                gr_fail("make_image: non-rectangular matrix",0);
                }
        }
        Begin_roots1(matrix)
                img = caml_gr_create_image(Val_int(width), Val_int(height));
        End_roots();
        has_transp = 0;
        oldBmp = SelectObject(grwindow.tempDC,Data(img));
        for (i = 0; i < height; i++) {
                for (j = 0; j < width; j++) {
                        int col = Long_val (Field (Field (matrix, i), j));
                        if (col == -1){
                                has_transp = 1;
                                SetPixel(grwindow.tempDC,j, i, 0);
                        }
                        else {
                                int red = (col >> 16) & 0xFF;
                                int green = (col >> 8) & 0xFF;
                                int blue = col & 0xFF;
                                SetPixel(grwindow.tempDC,j, i, RGB(red, green, blue));
                        }
                }
        }
        SelectObject(grwindow.tempDC,oldBmp);
        if (has_transp) {
                HBITMAP  cbm;
                cbm = CreateCompatibleBitmap(grwindow.gc, width, height);
                Mask(img) = cbm;
                oldBmp = SelectObject(grwindow.tempDC,Mask(img));
                for (i = 0; i < height; i++) {
                        for (j = 0; j < width; j++) {
                                int col = Long_val (Field (Field (matrix, i), j));
                                SetPixel(grwindow.tempDC,j, i, col == -1 ? 0xFFFFFF : 0);
                        }
                }
                SelectObject(grwindow.tempDC,oldBmp);
        }
        return img;
}

static value alloc_int_vect(mlsize_t size)
{
        value res;
        mlsize_t i;

        if (size == 0) return Atom(0);
        if (size <= Max_young_wosize) {
                res = alloc(size, 0);
        }
        else {
                res = alloc_shr(size, 0);
        }
        for (i = 0; i < size; i++) {
                Field(res, i) = Val_long(0);
        }
        return res;
}

CAMLprim value caml_gr_dump_image (value img)
{
        int height = Height(img);
        int width = Width(img);
        value matrix = Val_unit;
        int i, j;
        HBITMAP oldBmp;

        Begin_roots2(img, matrix)
                matrix = alloc_int_vect (height);
        for (i = 0; i < height; i++) {
                modify (&Field (matrix, i), alloc_int_vect (width));
        }
        End_roots();

        oldBmp = SelectObject(grwindow.tempDC,Data(img));
        for (i = 0; i < height; i++) {
                for (j = 0; j < width; j++) {
                        int col = GetPixel(grwindow.tempDC,j, i);
                        int blue = (col >> 16) & 0xFF;
                        int green = (col >> 8) & 0xFF;
                        int red = col & 0xFF;
                        Field(Field(matrix, i), j) = Val_long((red << 16) +
                                        (green << 8) + blue);
                }
        }
        SelectObject(grwindow.tempDC,oldBmp);
        if (Mask(img) != NULL) {
                oldBmp = SelectObject(grwindow.tempDC,Mask(img));
                for (i = 0; i < height; i++) {
                        for (j = 0; j < width; j++) {
                                if (GetPixel(grwindow.tempDC,j, i) != 0)
                                        Field(Field(matrix, i), j) =
                                                Val_long(-1);
                        }
                }
                SelectObject(grwindow.tempDC,oldBmp);
        }
        return matrix;
}
