#ifndef INCLUDED_PCRDATATYPE
#define INCLUDED_PCRDATATYPE

/*!
   \enum PCR_VS
   \brief "extended" value scales

   values scale/data types as used in PCRaster
   some such as VS_TABLE and VS_TSS are not  true
   value scales but it are other type of IDs, to
   discern between different uses of symbols
 */
typedef enum PCR_VS {
  VS_UNKNOWN=0, /**< scale is not known */
  VS_ERROR  =0, /**< scale is not known */
  VS_B=1,       /* boolean */
  VS_N=2,       /* nominal */
  VS_O=4,       /* ordinal */
  VS_S=8,       /* scalar */
  VS_D=16,      /* direction */
  VS_L=32,      /* ldd */
//VS_V=0,       /* vector  SET TO 0 disabled */
/* combined types legal in Fields 
 *  order of chars in name must be equal to order above
 *  code creation modules depend on this.
 */
  VS_BNOSDL=VS_B|VS_N|VS_O|VS_S|VS_D|VS_L,
  VS_FIELD=VS_BNOSDL,
  VS_BNO=VS_B|VS_N|VS_O,
  VS_BNOL=VS_BNO|VS_L,
  VS_NO=VS_N|VS_O,
  VS_NOSDL=VS_NO|VS_S|VS_D|VS_L,
  VS_OS=VS_O|VS_S,
//VS_SV=VS_S|VS_V,
  VS_SD=VS_S|VS_D,
  VS_BL=VS_B|VS_L,
/* other non-fields stuff */
  VS_TABLE=64,     /* a lookup table, THERE IS NO SUCH THING as a generic table */
  VS_TSS=128,      /* a timeseries */
  VS_ARRAY=256,    /* an array definition */
  VS_INDEX=512,    /* an array index */
  VS_STRING=1024,  /* a string  */
  VS_INDEXTABLE=2048,  /* a table used in index...() */
  VS_INDEXSET  =4096,
  VS_OBJECT =8192,
  VS_MAPSTACK  =16384,
  VS_STATISTICS  =32768,
  VS_NULL = 65536,  /* a Null value passed, e.g. a Python None object */

  VS_ASCIIFILE=VS_TABLE|VS_TSS|VS_INDEXTABLE,
  VS_NON_FIELD=VS_TABLE|VS_TSS|VS_ARRAY|VS_INDEX|VS_STRING|VS_STATISTICS,
  //! what can describe a number of indices
  VS_INDEX_CONTAINER=VS_ARRAY|VS_INDEX|VS_INDEXSET,
  //! what can belong to a set
  VS_INDEX_SUBSET=VS_INDEX|VS_INDEXSET,

  VS_ANYTHING=0xFFFFFFFF,
/*
 * csf version 1, types
 */
  VS_CLAS= VS_B|VS_N|VS_O|VS_L,  /* classified */
  VS_CONT= VS_S                  /* continuous */
  /* altough direction is a sub-type of continuous
   * it is not possible to default to that since
   * continuous (v1.0) has no knowledge on the new flat definition
   */
} PCR_VS;

/*! is the result always spatial or nonspatial or either?
 * \todo
 *   why are these enum types bit-sets?
 */
typedef enum PCR_ST { /* spatialtype */
  /*! result of empty ST intersection */
  ST_ERROR=0,
  /*!  must be spatial */
  ST_SPATIAL=1,
  /*!  must be non spatial */
  ST_NONSPATIAL=2,
  /*! both spatial or non-spatial is valid */
  ST_EITHER= ST_SPATIAL|ST_NONSPATIAL,
  /*! depends on operand, can be spatial or non-spatial */
  ST_DERIVED= ST_SPATIAL|ST_NONSPATIAL,
  //! not applicable, only used in oplist.sh for table and tss
  ST_NON = 4,
  ST_ALL = ST_EITHER|ST_NON
} PCR_ST;


typedef struct OP_ARGS {
   PCR_VS vs;
   PCR_ST st;
} OP_ARGS;

#endif
