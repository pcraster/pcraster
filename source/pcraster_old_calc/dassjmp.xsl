<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<xsl:output method="text"/>
<xsl:strip-space elements="*" />

<xsl:include href='common.xsl'/>


<xsl:template match="Operation[@exec='DOUBLE']">
  <xsl:text>{{</xsl:text>
    <xsl:for-each select="Result">
      <xsl:call-template name="opCode">
      <xsl:with-param name="str" select="concat(
                                          parent::Operation/@name
                                          ,@resultName)"/>
      </xsl:call-template>
      <xsl:if test='position()!=last()'>
         <xsl:text>,</xsl:text>
       </xsl:if>
      <xsl:text>&#xa;</xsl:text>
    </xsl:for-each>
  <xsl:value-of select="concat('},(DO_DASS)Do_',@name,'},')"/>
</xsl:template>

</xsl:transform>
