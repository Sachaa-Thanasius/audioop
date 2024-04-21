/* The audioop module uses the code base in g777.c file of the Sox project.
  Source: https://sourceforge.net/projects/sox/files/sox/12.17.7/sox-12.17.7.tar.gz

 Copyright of g771.c:

 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use.  Users may copy or modify this source code without
 * charge.
 *
 * SUN SOURCE CODE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING
 * THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS SOFTWARE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043  */

/* audioopmodule - Module to detect peak values in arrays */

// #define PY_SSIZE_T_CLEAN

// #include "Python.h"
#include "hpy.h"

static const int maxvals[] = {0, 0x7F, 0x7FFF, 0x7FFFFF, 0x7FFFFFFF};
/* -1 trick is needed on Windows to support -0x80000000 without a warning */
static const int minvals[] = {0, -0x80, -0x8000, -0x800000, -0x7FFFFFFF-1};
static const unsigned int masks[] = {0, 0xFF, 0xFFFF, 0xFFFFFF, 0xFFFFFFFF};

static int
fbound(double val, double minval, double maxval)
{
    if (val > maxval) {
        val = maxval;
    }
    else if (val < minval + 1.0) {
        val = minval;
    }

    /* Round towards minus infinity (-inf) */
    val = floor(val);

    /* Cast double to integer: round towards zero */
    return (int)val;
}


#define BIAS 0x84   /* define the add-in bias for 16 bit samples */
#define CLIP 32635
#define SIGN_BIT        (0x80)          /* Sign bit for an A-law byte. */
#define QUANT_MASK      (0xf)           /* Quantization field mask. */
#define SEG_SHIFT       (4)             /* Left shift for segment number. */
#define SEG_MASK        (0x70)          /* Segment field mask. */

static const int16_t seg_aend[8] = {
    0x1F, 0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF
};
static const int16_t seg_uend[8] = {
    0x3F, 0x7F, 0xFF, 0x1FF, 0x3FF, 0x7FF, 0xFFF, 0x1FFF
};

static int16_t
search(int16_t val, const int16_t *table, int size)
{
    assert(0 <= size);
    assert(size < INT16_MAX);
    int i;

    for (i = 0; i < size; i++) {
        if (val <= *table++)
            return (i);
    }
    return (size);
}
#define st_ulaw2linear16(uc) (_st_ulaw2linear16[uc])
#define st_alaw2linear16(uc) (_st_alaw2linear16[uc])

static const int16_t _st_ulaw2linear16[256] = {
    -32124,  -31100,  -30076,  -29052,  -28028,  -27004,  -25980,
    -24956,  -23932,  -22908,  -21884,  -20860,  -19836,  -18812,
    -17788,  -16764,  -15996,  -15484,  -14972,  -14460,  -13948,
    -13436,  -12924,  -12412,  -11900,  -11388,  -10876,  -10364,
     -9852,   -9340,   -8828,   -8316,   -7932,   -7676,   -7420,
     -7164,   -6908,   -6652,   -6396,   -6140,   -5884,   -5628,
     -5372,   -5116,   -4860,   -4604,   -4348,   -4092,   -3900,
     -3772,   -3644,   -3516,   -3388,   -3260,   -3132,   -3004,
     -2876,   -2748,   -2620,   -2492,   -2364,   -2236,   -2108,
     -1980,   -1884,   -1820,   -1756,   -1692,   -1628,   -1564,
     -1500,   -1436,   -1372,   -1308,   -1244,   -1180,   -1116,
     -1052,    -988,    -924,    -876,    -844,    -812,    -780,
      -748,    -716,    -684,    -652,    -620,    -588,    -556,
      -524,    -492,    -460,    -428,    -396,    -372,    -356,
      -340,    -324,    -308,    -292,    -276,    -260,    -244,
      -228,    -212,    -196,    -180,    -164,    -148,    -132,
      -120,    -112,    -104,     -96,     -88,     -80,     -72,
       -64,     -56,     -48,     -40,     -32,     -24,     -16,
    -8,       0,   32124,   31100,   30076,   29052,   28028,
     27004,   25980,   24956,   23932,   22908,   21884,   20860,
     19836,   18812,   17788,   16764,   15996,   15484,   14972,
     14460,   13948,   13436,   12924,   12412,   11900,   11388,
     10876,   10364,    9852,    9340,    8828,    8316,    7932,
      7676,    7420,    7164,    6908,    6652,    6396,    6140,
      5884,    5628,    5372,    5116,    4860,    4604,    4348,
      4092,    3900,    3772,    3644,    3516,    3388,    3260,
      3132,    3004,    2876,    2748,    2620,    2492,    2364,
      2236,    2108,    1980,    1884,    1820,    1756,    1692,
      1628,    1564,    1500,    1436,    1372,    1308,    1244,
      1180,    1116,    1052,     988,     924,     876,     844,
       812,     780,     748,     716,     684,     652,     620,
       588,     556,     524,     492,     460,     428,     396,
       372,     356,     340,     324,     308,     292,     276,
       260,     244,     228,     212,     196,     180,     164,
       148,     132,     120,     112,     104,      96,      88,
    80,      72,      64,      56,      48,      40,      32,
    24,      16,       8,       0
};

/*
 * linear2ulaw() accepts a 14-bit signed integer and encodes it as u-law data
 * stored in an unsigned char.  This function should only be called with
 * the data shifted such that it only contains information in the lower
 * 14-bits.
 *
 * In order to simplify the encoding process, the original linear magnitude
 * is biased by adding 33 which shifts the encoding range from (0 - 8158) to
 * (33 - 8191). The result can be seen in the following encoding table:
 *
 *      Biased Linear Input Code        Compressed Code
 *      ------------------------        ---------------
 *      00000001wxyza                   000wxyz
 *      0000001wxyzab                   001wxyz
 *      000001wxyzabc                   010wxyz
 *      00001wxyzabcd                   011wxyz
 *      0001wxyzabcde                   100wxyz
 *      001wxyzabcdef                   101wxyz
 *      01wxyzabcdefg                   110wxyz
 *      1wxyzabcdefgh                   111wxyz
 *
 * Each biased linear code has a leading 1 which identifies the segment
 * number. The value of the segment number is equal to 7 minus the number
 * of leading 0's. The quantization interval is directly available as the
 * four bits wxyz.  * The trailing bits (a - h) are ignored.
 *
 * Ordinarily the complement of the resulting code word is used for
 * transmission, and so the code word is complemented before it is returned.
 *
 * For further information see John C. Bellamy's Digital Telephony, 1982,
 * John Wiley & Sons, pps 98-111 and 472-476.
 */
static unsigned char
st_14linear2ulaw(int16_t pcm_val)       /* 2's complement (14-bit range) */
{
    int16_t         mask;
    int16_t         seg;
    unsigned char   uval;

    /* u-law inverts all bits */
    /* Get the sign and the magnitude of the value. */
    if (pcm_val < 0) {
        pcm_val = -pcm_val;
        mask = 0x7F;
    } else {
        mask = 0xFF;
    }
    if ( pcm_val > CLIP ) pcm_val = CLIP;           /* clip the magnitude */
    pcm_val += (BIAS >> 2);

    /* Convert the scaled magnitude to segment number. */
    seg = search(pcm_val, seg_uend, 8);

    /*
     * Combine the sign, segment, quantization bits;
     * and complement the code word.
     */
    if (seg >= 8)           /* out of range, return maximum value. */
        return (unsigned char) (0x7F ^ mask);
    else {
        assert(seg >= 0);
        uval = (unsigned char) (seg << 4) | ((pcm_val >> (seg + 1)) & 0xF);
        return (uval ^ mask);
    }

}

static const int16_t _st_alaw2linear16[256] = {
     -5504,   -5248,   -6016,   -5760,   -4480,   -4224,   -4992,
     -4736,   -7552,   -7296,   -8064,   -7808,   -6528,   -6272,
     -7040,   -6784,   -2752,   -2624,   -3008,   -2880,   -2240,
     -2112,   -2496,   -2368,   -3776,   -3648,   -4032,   -3904,
     -3264,   -3136,   -3520,   -3392,  -22016,  -20992,  -24064,
    -23040,  -17920,  -16896,  -19968,  -18944,  -30208,  -29184,
    -32256,  -31232,  -26112,  -25088,  -28160,  -27136,  -11008,
    -10496,  -12032,  -11520,   -8960,   -8448,   -9984,   -9472,
    -15104,  -14592,  -16128,  -15616,  -13056,  -12544,  -14080,
    -13568,    -344,    -328,    -376,    -360,    -280,    -264,
      -312,    -296,    -472,    -456,    -504,    -488,    -408,
      -392,    -440,    -424,     -88,     -72,    -120,    -104,
       -24,      -8,     -56,     -40,    -216,    -200,    -248,
      -232,    -152,    -136,    -184,    -168,   -1376,   -1312,
     -1504,   -1440,   -1120,   -1056,   -1248,   -1184,   -1888,
     -1824,   -2016,   -1952,   -1632,   -1568,   -1760,   -1696,
      -688,    -656,    -752,    -720,    -560,    -528,    -624,
      -592,    -944,    -912,   -1008,    -976,    -816,    -784,
      -880,    -848,    5504,    5248,    6016,    5760,    4480,
      4224,    4992,    4736,    7552,    7296,    8064,    7808,
      6528,    6272,    7040,    6784,    2752,    2624,    3008,
      2880,    2240,    2112,    2496,    2368,    3776,    3648,
      4032,    3904,    3264,    3136,    3520,    3392,   22016,
     20992,   24064,   23040,   17920,   16896,   19968,   18944,
     30208,   29184,   32256,   31232,   26112,   25088,   28160,
     27136,   11008,   10496,   12032,   11520,    8960,    8448,
      9984,    9472,   15104,   14592,   16128,   15616,   13056,
     12544,   14080,   13568,     344,     328,     376,     360,
       280,     264,     312,     296,     472,     456,     504,
       488,     408,     392,     440,     424,      88,      72,
       120,     104,      24,       8,      56,      40,     216,
       200,     248,     232,     152,     136,     184,     168,
      1376,    1312,    1504,    1440,    1120,    1056,    1248,
      1184,    1888,    1824,    2016,    1952,    1632,    1568,
      1760,    1696,     688,     656,     752,     720,     560,
       528,     624,     592,     944,     912,    1008,     976,
       816,     784,     880,     848
};

/*
 * linear2alaw() accepts a 13-bit signed integer and encodes it as A-law data
 * stored in an unsigned char.  This function should only be called with
 * the data shifted such that it only contains information in the lower
 * 13-bits.
 *
 *              Linear Input Code       Compressed Code
 *      ------------------------        ---------------
 *      0000000wxyza                    000wxyz
 *      0000001wxyza                    001wxyz
 *      000001wxyzab                    010wxyz
 *      00001wxyzabc                    011wxyz
 *      0001wxyzabcd                    100wxyz
 *      001wxyzabcde                    101wxyz
 *      01wxyzabcdef                    110wxyz
 *      1wxyzabcdefg                    111wxyz
 *
 * For further information see John C. Bellamy's Digital Telephony, 1982,
 * John Wiley & Sons, pps 98-111 and 472-476.
 */
static unsigned char
st_linear2alaw(int16_t pcm_val) /* 2's complement (13-bit range) */
{
    int16_t         mask;
    int16_t         seg;
    unsigned char   aval;

    /* A-law using even bit inversion */
    if (pcm_val >= 0) {
        mask = 0xD5;            /* sign (7th) bit = 1 */
    } else {
        mask = 0x55;            /* sign bit = 0 */
        pcm_val = -pcm_val - 1;
    }

    /* Convert the scaled magnitude to segment number. */
    seg = search(pcm_val, seg_aend, 8);

    /* Combine the sign, segment, and quantization bits. */

    if (seg >= 8)           /* out of range, return maximum value. */
        return (unsigned char) (0x7F ^ mask);
    else {
        aval = (unsigned char) seg << SEG_SHIFT;
        if (seg < 2)
            aval |= (pcm_val >> 1) & QUANT_MASK;
        else
            aval |= (pcm_val >> seg) & QUANT_MASK;
        return (aval ^ mask);
    }
}
/* End of code taken from sox */

/* Intel ADPCM step variation table */
static const int indexTable[16] = {
    -1, -1, -1, -1, 2, 4, 6, 8,
    -1, -1, -1, -1, 2, 4, 6, 8,
};

static const int stepsizeTable[89] = {
    7, 8, 9, 10, 11, 12, 13, 14, 16, 17,
    19, 21, 23, 25, 28, 31, 34, 37, 41, 45,
    50, 55, 60, 66, 73, 80, 88, 97, 107, 118,
    130, 143, 157, 173, 190, 209, 230, 253, 279, 307,
    337, 371, 408, 449, 494, 544, 598, 658, 724, 796,
    876, 963, 1060, 1166, 1282, 1411, 1552, 1707, 1878, 2066,
    2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358,
    5894, 6484, 7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899,
    15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
};

#define GETINTX(T, cp, i)  (*(T *)((unsigned char *)(cp) + (i)))
#define SETINTX(T, cp, i, val)  do {                    \
        *(T *)((unsigned char *)(cp) + (i)) = (T)(val); \
    } while (0)


#define GETINT8(cp, i)          GETINTX(signed char, (cp), (i))
#define GETINT16(cp, i)         GETINTX(int16_t, (cp), (i))
#define GETINT32(cp, i)         GETINTX(int32_t, (cp), (i))

#ifdef WORDS_BIGENDIAN
#define GETINT24(cp, i)  (                              \
        ((unsigned char *)(cp) + (i))[2] +              \
        (((unsigned char *)(cp) + (i))[1] * (1 << 8)) + \
        (((signed char *)(cp) + (i))[0] * (1 << 16)) )
#else
#define GETINT24(cp, i)  (                              \
        ((unsigned char *)(cp) + (i))[0] +              \
        (((unsigned char *)(cp) + (i))[1] * (1 << 8)) + \
        (((signed char *)(cp) + (i))[2] * (1 << 16)) )
#endif


#define SETINT8(cp, i, val)     SETINTX(signed char, (cp), (i), (val))
#define SETINT16(cp, i, val)    SETINTX(int16_t, (cp), (i), (val))
#define SETINT32(cp, i, val)    SETINTX(int32_t, (cp), (i), (val))

#ifdef WORDS_BIGENDIAN
#define SETINT24(cp, i, val)  do {                              \
        ((unsigned char *)(cp) + (i))[2] = (int)(val);          \
        ((unsigned char *)(cp) + (i))[1] = (int)(val) >> 8;     \
        ((signed char *)(cp) + (i))[0] = (int)(val) >> 16;      \
    } while (0)
#else
#define SETINT24(cp, i, val)  do {                              \
        ((unsigned char *)(cp) + (i))[0] = (int)(val);          \
        ((unsigned char *)(cp) + (i))[1] = (int)(val) >> 8;     \
        ((signed char *)(cp) + (i))[2] = (int)(val) >> 16;      \
    } while (0)
#endif


#define GETRAWSAMPLE(size, cp, i)  (                    \
        (size == 1) ? (int)GETINT8((cp), (i)) :         \
        (size == 2) ? (int)GETINT16((cp), (i)) :        \
        (size == 3) ? (int)GETINT24((cp), (i)) :        \
                      (int)GETINT32((cp), (i)))

#define SETRAWSAMPLE(size, cp, i, val)  do {    \
        if (size == 1)                          \
            SETINT8((cp), (i), (val));          \
        else if (size == 2)                     \
            SETINT16((cp), (i), (val));         \
        else if (size == 3)                     \
            SETINT24((cp), (i), (val));         \
        else                                    \
            SETINT32((cp), (i), (val));         \
    } while(0)


#define GETSAMPLE32(size, cp, i)  (                           \
        (size == 1) ? (int)GETINT8((cp), (i)) * (1 << 24) :   \
        (size == 2) ? (int)GETINT16((cp), (i)) * (1 << 16) :  \
        (size == 3) ? (int)GETINT24((cp), (i)) * (1 << 8) :   \
                      (int)GETINT32((cp), (i)))

#define SETSAMPLE32(size, cp, i, val)  do {     \
        if (size == 1)                          \
            SETINT8((cp), (i), (val) >> 24);    \
        else if (size == 2)                     \
            SETINT16((cp), (i), (val) >> 16);   \
        else if (size == 3)                     \
            SETINT24((cp), (i), (val) >> 8);    \
        else                                    \
            SETINT32((cp), (i), (val));         \
    } while(0)

static HPyGlobal AudioopError;

static int
audioop_check_size(HPyContext *ctx, HPy module, int size)
{
    if (size < 1 || size > 4) {
        HPy h_AudioopError = HPyGlobal_Load(ctx, AudioopError);
        HPyErr_SetString(ctx, h_AudioopError,
                        "Size should be 1, 2, 3 or 4");
        HPy_Close(ctx, h_AudioopError);
        return 0;
    }
    else
        return 1;
}

static int
audioop_check_parameters(HPyContext *ctx, HPy module, HPy_ssize_t len, int size)
{
    if (!audioop_check_size(ctx, module, size))
        return 0;
    if (len % size != 0) {
        HPy h_AudioopError = HPyGlobal_Load(ctx, AudioopError);
        HPyErr_SetString(ctx, h_AudioopError,
                        "not a whole number of frames");
        HPy_Close(ctx, h_AudioopError);
        return 0;
    }
    return 1;
}

int
_LTS_PyArg_CheckPositional(const char *name, Py_ssize_t nargs,
                       Py_ssize_t min, Py_ssize_t max)
{
    assert(min >= 0);
    assert(min <= max);

    if (nargs < min) {
        if (name != NULL)
            PyErr_Format(
                PyExc_TypeError,
                "%.200s expected %s%zd argument%s, got %zd",
                name, (min == max ? "" : "at least "), min, min == 1 ? "" : "s", nargs);
        else
            PyErr_Format(
                PyExc_TypeError,
                "unpacked tuple should have %s%zd element%s,"
                " but has %zd",
                (min == max ? "" : "at least "), min, min == 1 ? "" : "s", nargs);
        return 0;
    }

    if (nargs == 0) {
        return 1;
    }

    if (nargs > max) {
        if (name != NULL)
            PyErr_Format(
                PyExc_TypeError,
                "%.200s expected %s%zd argument%s, got %zd",
                name, (min == max ? "" : "at most "), max, max == 1 ? "" : "s", nargs);
        else
            PyErr_Format(
                PyExc_TypeError,
                "unpacked tuple should have %s%zd element%s,"
                " but has %zd",
                (min == max ? "" : "at most "), max, max == 1 ? "" : "s", nargs);
        return 0;
    }

    return 1;
}

void
_HPyArg_BadArgument(HPyContext *ctx, const char *fname, const char *displayname,
                   const char *expected, HPy arg)
{
    HPyErr_Format(ctx, ctx->h_TypeError,
                    "%.200s() %.200s must be %.50s, not %.50s",
                    fname, displayname, expected,
                    HPy_Is(ctx, arg, ctx->h_None) ? "None" : HPyType_GetName(ctx, arg));
    
}


static HPy
_audioop_getsample_inner(HPyContext *ctx, HPy module, HPy_buffer *fragment, int width,
                       HPy_ssize_t index)
/*[clinic end generated code: output=8fe1b1775134f39a input=88edbe2871393549]*/
{
    int val;

    if (!audioop_check_parameters(ctx, module, fragment->len, width))
        return HPy_NULL;
    if (index < 0 || index >= fragment->len/width) {
        HPy h_AudioopError = HPyGlobal_Load(ctx, AudioopError);
        HPyErr_SetString(ctx, h_AudioopError, "Index out of range");
        HPy_Close(ctx, h_AudioopError);
        return HPy_NULL;
    }
    val = GETRAWSAMPLE(width, fragment->buf, index*width);
    return HPyLong_FromLong(ctx, val);
}

static HPy
audioop_getsample_impl(HPyContext *ctx, HPy module, HPy_buffer *fragment, int width,
                       HPy_ssize_t index);

HPyDef_METH(audioop_getsample_test, "audioop_getsample_test", HPyFunc_VARARGS,
.doc="getsample($module, fragment, width, index, /)\n"
"--\n"
"\n"
"Return the value of sample index from the fragment."
)
static HPy
audioop_getsample_test_impl(HPyContext *ctx, HPy module, const HPy *args, size_t nargs)
{
    HPy return_value = HPy_NULL;

    HPy h_frag;
    int width;
    HPy h_index;

    Py_buffer fragment = {NULL, NULL};
    HPy_ssize_t index;

    if (!HPyArg_Parse(ctx, NULL, args, nargs, "OiO", &h_frag, &width, &h_index)) {
        goto exit;
    }

    PyObject *py_buff = HPy_AsPyObject(ctx, h_frag);
    if (PyObject_GetBuffer(py_buff, &fragment, PyBUF_SIMPLE) != 0) {
        goto exit;
    }
    if (!PyBuffer_IsContiguous(&fragment, 'C')) {
        _HPyArg_BadArgument(ctx, "getsample", "argument 1", "contiguous buffer", args[0]);
        goto exit;
    }

    if (width == -1 && HPyErr_Occurred(ctx)) {
        goto exit;
    }

    {
        HPy_ssize_t ival = -1;
        HPy iobj = HPy_Index(ctx, args[2]);
        if (HPy_IsNull(iobj)) {
            ival = HPyLong_AsSsize_t(ctx, iobj);
        }

        if (ival == -1 && HPyErr_Occurred(ctx)) {
            goto exit;
        }
        index = ival;
    }

    return_value = _audioop_getsample_inner(ctx, module, (HPy_buffer *) &fragment, width, index);

exit:
    /* Cleanup for fragment */
    if (fragment.obj) {
       PyBuffer_Release(&fragment);
    }

    return return_value;
}

HPyDef_SLOT(audioop_exec, HPy_mod_exec)
static int
audioop_exec_impl(HPyContext *ctx, HPy hm)
{
    HPy h_AudioopError = HPyErr_NewException(ctx, "audioop.error", HPy_NULL, HPy_NULL);
    if (HPy_IsNull(h_AudioopError)) {
        return 1;
    }

    if (HPy_SetAttr_s(ctx, hm, "error", h_AudioopError) != 0) {
        HPy_Close(ctx, h_AudioopError);
        return 1;
    }

    HPyGlobal_Store(ctx, &AudioopError, h_AudioopError);
    return 0;
}

static HPyDef *audioop_defines[] = {
    &audioop_exec,
    // Need methods from audioop_methods in here.
    NULL
};

static HPyGlobal *audioop_globals[] = {
    &AudioopError,
    NULL
};

static HPyModuleDef audioopmodule = {
    .doc = "audioop implemented with the HPy API.",
    .defines = audioop_defines,
    .globals = audioop_globals,
};

HPy_MODINIT(audioop, audioopmodule)