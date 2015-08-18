<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<xsl:output method="xml"/>
<xsl:strip-space elements="*" />

<!-- phase 1 of 2 to create major_op.h
     output  Names = Name+ 
 -->

<xsl:include href='common.xsl'/>

<xsl:template match="Operations">
<Names>
 <xsl:apply-templates/>
</Names>
</xsl:template>

<!-- specific -->
<xsl:template match="Operation[@implementationName]">
 <Name>
  <xsl:value-of select="@implementationName"/>
 </Name>
 <xsl:apply-templates/>
</xsl:template>

<!-- less specific -->
<xsl:template match="Operation">
 <Name>
  <xsl:value-of select="@name"/>
 </Name>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="Implementation">
  <xsl:variable name="prefix">
    <xsl:value-of select="substring(Field/DataType/@value,1,1)"/>
  </xsl:variable>
 <Name>
   <xsl:value-of select="concat(parent::Operation/@name,'_',$prefix)"/>
 </Name>
</xsl:template>

</xsl:transform>
<!--
/* this file is created by oplist. do not edit */
/* major opcodes */
#ifndef INCLUDED_MAJOR_OP
#define INCLUDED_MAJOR_OP
typedef enum MAJOR_CODE { 
OP_NOP=0 ,
OP_ILL=1 ,
OP_IF_ELSE=2 ,
   <Implementation exec='SAME_BIN'><Field>&IMPL_4;</Field></Implementation>
.......
OP_DYNAMICWAVE=249 
} MAJOR_CODE ; 
#endif /* INCLUDED_MAJOR_OP */
-->
