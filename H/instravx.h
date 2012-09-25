/****************************************************************************
*
* Description:  AVX instructions with VEX prefix
*
****************************************************************************/

#if AVXSUPP
/*   tok                    len     cpu */
/* format: xmm|ymm, xmm|ymm|mem */
avxins (ADDPD,    vaddpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (ADDPS,    vaddps,    6,     P_AVX, VX_L ) /* L, s */
avxins (ADDSD,    vaddsd,    6,     P_AVX, 0 )    /* -, s */
avxins (ADDSS,    vaddss,    6,     P_AVX, 0 )    /* -, s */
avxins (DIVPD,    vdivpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (DIVPS,    vdivps,    6,     P_AVX, VX_L ) /* L, s */
avxins (DIVSD,    vdivsd,    6,     P_AVX, 0 )    /* -, s */
avxins (DIVSS,    vdivss,    6,     P_AVX, 0 )    /* -, s */
avxins (MAXPD,    vmaxpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (MAXPS,    vmaxps,    6,     P_AVX, VX_L ) /* L, s */
avxins (MAXSD,    vmaxsd,    6,     P_AVX, 0 )    /* -, s */
avxins (MAXSS,    vmaxss,    6,     P_AVX, 0 )    /* -, s */
avxins (MINPD,    vminpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (MINPS,    vminps,    6,     P_AVX, VX_L ) /* L, s */
avxins (MINSD,    vminsd,    6,     P_AVX, 0 )    /* -, s */
avxins (MINSS,    vminss,    6,     P_AVX, 0 )    /* -, s */
avxins (MULPD,    vmulpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (MULPS,    vmulps,    6,     P_AVX, VX_L ) /* L, s */
avxins (MULSD,    vmulsd,    6,     P_AVX, 0 )    /* -, s */
avxins (MULSS,    vmulss,    6,     P_AVX, 0 )    /* -, s */
avxins (SQRTPD,   vsqrtpd,   7,     P_AVX, VX_L ) /* L, s */
avxins (SQRTPS,   vsqrtps,   7,     P_AVX, VX_L ) /* L, s */
avxins (SQRTSD,   vsqrtsd,   7,     P_AVX, 0 )    /* -, s */
avxins (SQRTSS,   vsqrtss,   7,     P_AVX, 0 )    /* -, s */
avxins (SUBPD,    vsubpd,    6,     P_AVX, VX_L ) /* L, s */
avxins (SUBPS,    vsubps,    6,     P_AVX, VX_L ) /* L, s */
avxins (SUBSD,    vsubsd,    6,     P_AVX, 0 )    /* -, s */
avxins (SUBSS,    vsubss,    6,     P_AVX, 0 )    /* -, s */
avxins (CMPPD,    vcmppd,    6,     P_AVX, VX_L ) /* L, s */
avxins (CMPPS,    vcmpps,    6,     P_AVX, VX_L ) /* L, s */
avxins (CMPSD,    vcmpsd,    6,     P_AVX, 0 )    /* -, s */
avxins (CMPSS,    vcmpss,    6,     P_AVX, 0 )    /* -, s */

/* format: xmm|ymm, xmm|ymm|mem */
avxins (ANDPD,    vandpd,    6,     P_AVX, VX_L )   /* L, s */
avxins (ANDPS,    vandps,    6,     P_AVX, VX_L )   /* L, s */
avxins (ANDNPD,   vandnpd,   7,     P_AVX, VX_L )   /* L, s */
avxins (ANDNPS,   vandnps,   7,     P_AVX, VX_L )   /* L, s */
avxins (ORPD,     vorpd,     5,     P_AVX, VX_L )   /* L, s */
avxins (ORPS,     vorps,     5,     P_AVX, VX_L )   /* L, s */
avxins (COMISD,   vcomisd,   7,     P_AVX, VX_NND ) /* -, ns */
avxins (COMISS,   vcomiss,   7,     P_AVX, VX_NND ) /* -, ns */
avxins (XORPD,    vxorpd,    6,     P_AVX, VX_L )   /* L, s */
avxins (XORPS,    vxorps,    6,     P_AVX, VX_L )   /* L, s */

/* format: xmm|ymm, xmm|ymm|mem */
avxins (CVTDQ2PD, vcvtdq2pd, 9,     P_AVX, VX_L|VX_NND|VX_HALF ) /* L, ns, 64->128 */
avxins (CVTDQ2PS, vcvtdq2ps, 9,     P_AVX, VX_L|VX_NND ) /* L, ns */
//avxins (CVTPD2DQ, vcvtpd2dq, 9,     P_AVX, VX_L|VX_NND ) /* L, ns */
//avxins (CVTTPD2DQ,vcvttpd2dq,10,    P_AVX, VX_L|VX_NND ) /* L, ns */
//avxins (CVTPD2PS, vcvtpd2ps, 9,     P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (CVTPS2DQ, vcvtps2dq, 9,     P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (CVTTPS2DQ,vcvttps2dq,10,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (CVTPS2PD, vcvtps2pd, 9,     P_AVX, VX_L|VX_NND|VX_HALF ) /* L, ns, 64->128 */
avxins (CVTSD2SI, vcvtsd2si, 9,     P_AVX, VX_NND )   /* -, ns, W */
avxins (CVTTSD2SI,vcvttsd2si,10,    P_AVX, VX_NND )   /* -, ns, W */
avxins (CVTSD2SS, vcvtsd2ss, 9,     P_AVX, 0 )        /* -, s     */
avxins (CVTSI2SD, vcvtsi2sd, 9,     P_AVX, 0 )        /* -, s,  W */
avxins (CVTSI2SS, vcvtsi2ss, 9,     P_AVX, 0 )        /* -, s,  W */
avxins (CVTSS2SD, vcvtss2sd, 9,     P_AVX, 0 )        /* -, s     */
avxins (CVTSS2SI, vcvtss2si, 9,     P_AVX, VX_NND )   /* -, ns, W */
avxins (CVTTSS2SI,vcvttss2si,10,    P_AVX, VX_NND )   /* -, ns, W */

/* format: xmm|ymm, xmm|ymm|mem [, i8] */
avxins (ADDSUBPD, vaddsubpd, 9,     P_AVX, VX_L )     /* L, s */
avxins (ADDSUBPS, vaddsubps, 9,     P_AVX, VX_L )     /* L, s */
avxins (BLENDPD , vblendpd , 8,     P_AVX, VX_L )     /* L, s */
avxins (BLENDPS , vblendps , 8,     P_AVX, VX_L )     /* L, s */
avxins (DPPD    , vdppd    , 5,     P_AVX, 0 )        /* -, s */
avxins (DPPS    , vdpps    , 5,     P_AVX, VX_L )     /* L, s */
avxins (EXTRACTPS,vextractps,10,    P_AVX, VX_NND )   /* -, ns! */ /* format: reg|mem32, xmm|ymm, i8 */
avxins (HADDPD  , vhaddpd  , 7,     P_AVX, VX_L )     /* L, s */
avxins (HADDPS  , vhaddps  , 7,     P_AVX, VX_L )     /* L, s */
avxins (HSUBPD  , vhsubpd  , 7,     P_AVX, VX_L )     /* L, s */
avxins (HSUBPS  , vhsubps  , 7,     P_AVX, VX_L )     /* L, s */
avxins (INSERTPS, vinsertps, 9,     P_AVX, 0 )        /* -, s */
avxins (LDDQU   , vlddqu   , 6,     P_AVX, VX_L|VX_NND ) /* L, ns */ /* format: xmm|ymm, mem */
avxins (LDMXCSR , vldmxcsr , 8,     P_AVX, 0 )        /* -, ns */ /* format: mem32 */
avxins (STMXCSR , vstmxcsr , 8,     P_AVX, 0 )        /* -, ns */ /* format: mem32 */

avxins (MASKMOVDQU,vmaskmovdqu,11,  P_AVX, VX_NND )   /* -, ns */
avxins (MOVAPD  , vmovapd  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVAPS  , vmovaps  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVD    , vmovd    ,  5,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVQ    , vmovq    ,  5,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVDQA  , vmovdqa  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVDQU  , vmovdqu  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVHLPS , vmovhlps ,  8,    P_AVX, 0 )        /* -, s */
avxins (MOVLHPS , vmovlhps ,  8,    P_AVX, 0 )        /* -, s */
avxins (MOVHPD  , vmovhpd  ,  7,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVHPS  , vmovhps  ,  7,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVLPD  , vmovlpd  ,  7,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVLPS  , vmovlps  ,  7,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVSD   , vmovsd   ,  6,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVSS   , vmovss   ,  6,    P_AVX, VX_NMEM )  /* -, s/ns! */
avxins (MOVMSKPD, vmovmskpd,  9,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVMSKPS, vmovmskps,  9,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVNTDQ , vmovntdq ,  8,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVNTDQA, vmovntdqa,  9,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVNTPD , vmovntpd ,  8,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVNTPS , vmovntps ,  8,    P_AVX, VX_NND )   /* -, ns */
avxins (MOVSHDUP, vmovshdup,  9,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVSLDUP, vmovsldup,  9,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVUPD  , vmovupd  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (MOVUPS  , vmovups  ,  7,    P_AVX, VX_L|VX_NND ) /* L, ns */

avxins (MPSADBW , vmpsadbw ,  8,    P_AVX, 0 )        /* -, s */

avxins (PABSB   , vpabsb   ,  6,    P_AVX, VX_NND )   /* -, ns */
avxins (PABSW   , vpabsw   ,  6,    P_AVX, VX_NND )   /* -, ns */
avxins (PABSD   , vpabsd   ,  6,    P_AVX, VX_NND )   /* -, ns */
avxins (PACKSSWB, vpacksswb,  9,    P_AVX, 0 )        /* -, s */
avxins (PACKSSDW, vpackssdw,  9,    P_AVX, 0 )        /* -, s */
avxins (PACKUSWB, vpackuswb,  9,    P_AVX, 0 )        /* -, s */
avxins (PACKUSDW, vpackusdw,  9,    P_AVX, 0 )        /* -, s */
avxins (PADDB   , vpaddb   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PADDW   , vpaddw   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PADDD   , vpaddd   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PADDQ   , vpaddq   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PADDSB  , vpaddsb  ,  7,    P_AVX, 0 )        /* -, s */
avxins (PADDSW  , vpaddsw  ,  7,    P_AVX, 0 )        /* -, s */
avxins (PADDUSB , vpaddusb ,  8,    P_AVX, 0 )        /* -, s */
avxins (PADDUSW , vpaddusw ,  8,    P_AVX, 0 )        /* -, s */
avxins (PALIGNR , vpalignr ,  8,    P_AVX, 0 )        /* -, s */
avxins (PAND    , vpand    ,  5,    P_AVX, 0 )        /* -, s */
avxins (PANDN   , vpandn   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PAVGB   , vpavgb   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PAVGW   , vpavgw   ,  6,    P_AVX, 0 )        /* -, s */
avxins (PBLENDW , vpblendw ,  8,    P_AVX, 0 )        /* -, s */
avxins (PCMPESTRI,vpcmpestri,10,    P_AVX, VX_NND )   /* -, ns! */
avxins (PCMPESTRM,vpcmpestrm,10,    P_AVX, VX_NND )   /* -, ns! */
avxins (PCMPISTRI,vpcmpistri,10,    P_AVX, VX_NND )   /* -, ns! */
avxins (PCMPISTRM,vpcmpistrm,10,    P_AVX, VX_NND )   /* -, ns! */
avxins (PCMPEQB  ,vpcmpeqb  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPEQW  ,vpcmpeqw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPEQD  ,vpcmpeqd  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPEQQ  ,vpcmpeqq  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPGTB  ,vpcmpgtB  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPGTW  ,vpcmpgtw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPGTD  ,vpcmpgtd  , 8,    P_AVX, 0 )        /* -, s */
avxins (PCMPGTQ  ,vpcmpgtq  , 8,    P_AVX, 0 )        /* -, s */
avxins (PEXTRB   ,vpextrb   , 7,    P_AVX, VX_NND )   /* -, ns! */
avxins (PEXTRW   ,vpextrw   , 7,    P_AVX, VX_NND )   /* -, ns! */
avxins (PEXTRD   ,vpextrd   , 7,    P_AVX, VX_NND )   /* -, ns! */
avxins (PINSRB   ,vpinsrb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PINSRW   ,vpinsrw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PINSRD   ,vpinsrd   , 7,    P_AVX, 0 )        /* -, s */
#if AMD64_SUPPORT
avxins (PEXTRQ   ,vpextrq   , 7,    P_AVX, VX_NND )   /* -, ns! */
avxins (PINSRQ   ,vpinsrq   , 7,    P_AVX, 0 )        /* -, s */
#endif
avxins (PHADDW   ,vphaddw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PHADDD   ,vphaddd   , 7,    P_AVX, 0 )        /* -, s */
avxins (PHADDSW  ,vphaddsw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PHMINPOSUW,vphminposuw,11,  P_AVX, VX_NND )   /* -, ns */
avxins (PHSUBW   ,vphsubw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PHSUBD   ,vphsubd   , 7,    P_AVX, 0 )        /* -, s */
avxins (PHSUBSW  ,vphsubsw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PMADDWD  ,vpmaddwd  , 8,    P_AVX, 0 )        /* -, s */
avxins (PMADDUBSW,vpmaddubsw,10,    P_AVX, 0 )        /* -, s */
avxins (PMAXSB   ,vpmaxsb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMAXSW   ,vpmaxsw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMAXSD   ,vpmaxsd   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMAXUB   ,vpmaxub   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMAXUW   ,vpmaxuw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMAXUD   ,vpmaxud   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINSB   ,vpminsb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINSW   ,vpminsw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINSD   ,vpminsd   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINUB   ,vpminub   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINUW   ,vpminuw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMINUD   ,vpminud   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMOVMSKB ,vpmovmskb , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXBW ,vpmovsxbw , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXBD ,vpmovsxbd , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXBQ ,vpmovsxbq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXWD ,vpmovsxwd , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXWQ ,vpmovsxwq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVSXDQ ,vpmovsxdq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXBW ,vpmovzxbw , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXBD ,vpmovzxbd , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXBQ ,vpmovzxbq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXWD ,vpmovzxwd , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXWQ ,vpmovzxwq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMOVZXDQ ,vpmovzxdq , 9,    P_AVX, VX_NND )   /* -, ns */
avxins (PMULHUW  ,vpmulhuw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PMULHRSW ,vpmulhrsw , 9,    P_AVX, 0 )        /* -, s */
avxins (PMULHW   ,vpmulhw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMULLW   ,vpmullw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMULLD   ,vpmulld   , 7,    P_AVX, 0 )        /* -, s */
avxins (PMULUDQ  ,vpmuludq  , 8,    P_AVX, 0 )        /* -, s */
avxins (PMULDQ   ,vpmuldq   , 7,    P_AVX, 0 )        /* -, s */
avxins (POR      ,vpor      , 4,    P_AVX, 0 )        /* -, s */
avxins (PSADBW   ,vpsadbw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSHUFB   ,vpshufb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSHUFD   ,vpshufd   , 7,    P_AVX, VX_NND )   /* -, ns! */
avxins (PSHUFHW  ,vpshufhw  , 8,    P_AVX, VX_NND )   /* -, ns! */
avxins (PSHUFLW  ,vpshuflw  , 8,    P_AVX, VX_NND )   /* -, ns! */
avxins (PSIGNB   ,vpsignb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSIGNW   ,vpsignw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSIGND   ,vpsignd   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSLLDQ   ,vpslldq   , 7,    P_AVX, VX_DST )   /* -, d */
avxins (PSRLDQ   ,vpsrldq   , 7,    P_AVX, VX_DST )   /* -, d */
avxins (PSLLW    ,vpsllw    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSLLD    ,vpslld    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSLLQ    ,vpsllq    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSRAW    ,vpsraw    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSRAD    ,vpsrad    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSRLW    ,vpsrlw    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSRLD    ,vpsrld    , 6,    P_AVX, VX_DST )   /* -, d/s */
avxins (PSRLQ    ,vpsrlq    , 6,    P_AVX, VX_DST )   /* -, d/s */
/* */
avxins (PTEST    ,vptest    , 6,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (PSUBB    ,vpsubb    , 6,    P_AVX, 0 )        /* -, s */
avxins (PSUBW    ,vpsubw    , 6,    P_AVX, 0 )        /* -, s */
avxins (PSUBD    ,vpsubd    , 6,    P_AVX, 0 )        /* -, s */
avxins (PSUBQ    ,vpsubq    , 6,    P_AVX, 0 )        /* -, s */
avxins (PSUBSB   ,vpsubsb   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSUBSW   ,vpsubsw   , 7,    P_AVX, 0 )        /* -, s */
avxins (PSUBUSB  ,vpsubusb  , 8,    P_AVX, 0 )        /* -, s */
avxins (PSUBUSW  ,vpsubusw  , 8,    P_AVX, 0 )        /* -, s */
avxins (PUNPCKHBW ,vpunpckhbw, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKHWD ,vpunpckhwd, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKHDQ ,vpunpckhdq, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKHQDQ,vpunpckhqdq,11,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKLBW ,vpunpcklbw, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKLWD ,vpunpcklwd, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKLDQ ,vpunpckldq, 10,  P_AVX, 0 )        /* -, s */
avxins (PUNPCKLQDQ,vpunpcklqdq,11,  P_AVX, 0 )        /* -, s */
avxins (PXOR     ,vpxor     , 5,    P_AVX, 0 )        /* -, s */

avxins (RCPPS    ,vrcpps    , 6,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (RCPSS    ,vrcpss    , 6,    P_AVX, 0 )        /* -, s */
avxins (RSQRTPS  ,vrsqrtps  , 8,    P_AVX, VX_L|VX_NND ) /* L, ns */
avxins (RSQRTSS  ,vrsqrtss  , 8,    P_AVX, 0 )        /* -, s */
avxins (ROUNDPD  ,vroundpd  , 8,    P_AVX, VX_L|VX_NND ) /* L, ns! */
avxins (ROUNDPS  ,vroundps  , 8,    P_AVX, VX_L|VX_NND ) /* L, ns! */
avxins (ROUNDSD  ,vroundsd  , 8,    P_AVX, 0 )        /* -, s */
avxins (ROUNDSS  ,vroundss  , 8,    P_AVX, 0 )        /* -, s */
avxins (SHUFPD   ,vshufpd   , 7,    P_AVX, VX_L )     /* L, s */
avxins (SHUFPS   ,vshufps   , 7,    P_AVX, VX_L )     /* L, s */
avxins (UCOMISD  ,vucomisd  , 8,    P_AVX, VX_NND )   /* -, ns */
avxins (UCOMISS  ,vucomiss  , 8,    P_AVX, VX_NND )   /* -, ns */
avxins (UNPCKHPD ,vunpckhpd , 9,    P_AVX, VX_L )     /* L, s */
avxins (UNPCKHPS ,vunpckhps , 9,    P_AVX, VX_L )     /* L, s */
avxins (UNPCKLPD ,vunpcklpd , 9,    P_AVX, VX_L )     /* L, s */
avxins (UNPCKLPS ,vunpcklps , 9,    P_AVX, VX_L )     /* L, s */
#endif

//avxins (PCLMULQDQ,vpclmulqdq,10,    P_AVX, 0 )
