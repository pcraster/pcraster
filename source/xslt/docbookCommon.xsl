<?xml version='1.0'?>
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:param name="language" select="'en'"></xsl:param>
  <xsl:param name="section.autolabel" select="1"/>
  <xsl:param name="section.label.includes.component.label" select="1"/>
  <xsl:param name="xref.with.number.and.title" select="0"/>
  <xsl:param name="generate.toc" select="'book toc,title,figure,table,example'"/>
  <xsl:param name="bibliography.numbered" select="1"/>

  <xsl:template match="pcr-go">
    <xsl:text>--</xsl:text>
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>