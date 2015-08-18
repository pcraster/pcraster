<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- xsltproc like it like this -->
<xsl:output method="xml" indent="yes"/>
<xsl:strip-space elements="*"/>
 <xsl:template match="/">
    <xsl:copy-of select="."/>
 </xsl:template>
</xsl:stylesheet>

<!--
 we had this
<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<xsl:output method="xml" indent="yes"/>
<xsl:template match="/"><xsl:copy-of select="."/></xsl:template>
</xsl:transform>
-->
