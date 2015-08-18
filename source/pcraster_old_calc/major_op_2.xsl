<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<!-- input  Names = Name+  from major_op_1.xsl
     generate major_op.h
 -->

<xsl:output method="text"/>
<xsl:strip-space elements="*" />

<xsl:include href='common.xsl'/>

<xsl:template match="Names">
<xsl:text>
/* this file is created by oplist. do not edit */
/* major opcodes */
#ifndef INCLUDED_MAJOR_OP
#define INCLUDED_MAJOR_OP
typedef enum MAJOR_CODE {
</xsl:text>
<xsl:apply-templates/>
<xsl:text>
} MAJOR_CODE ;
#endif /* INCLUDED_MAJOR_OP */
</xsl:text>

</xsl:template>


<xsl:template match="Name">
  <xsl:call-template name="opCode">
    <xsl:with-param name="str" select="concat(
                                          .,
                                          '=',
                                          position()
                                          )"/>
   </xsl:call-template>
   <xsl:if test='position()!=last()'>
    <xsl:text>,&#xa;</xsl:text>
   </xsl:if>
</xsl:template>

</xsl:transform>
