<xsl:stylesheet
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>

<xsl:template name="pcr-toupper">
  <xsl:param name="str"/>
      <xsl:value-of select="translate($str,
      'abcdefghijklmnopqrstuvwxyz',
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
</xsl:template>

<xsl:template name="opCode">
  <xsl:param name="str"/>
      <xsl:value-of select="concat('OP_',translate($str,
      'abcdefghijklmnopqrstuvwxyz',
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'))"/>
</xsl:template>

</xsl:stylesheet>
