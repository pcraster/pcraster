<?xml version='1.0'?>
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format"
  version="1.0">

  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/fo/docbook.xsl"/>
  <xsl:include href="docbookCommon.xsl"/>

  <xsl:param name="paper.type" select="'A4'"/>
  <xsl:param name="body.start.indent" select="0"/>
  <xsl:param name="page.margin.inner" select="'1.8cm'"/>
  <xsl:param name="page.margin.outer" select="'1.5cm'"/>
  <xsl:param name="fop1.extensions" select="1"/>

  <xsl:template match="pcr-op">
    <fo:inline font-weight="bold"><xsl:apply-templates/></fo:inline>
  </xsl:template>

  <xsl:template match="br">
    <xsl:text>&#xA;</xsl:text>
  </xsl:template>

  <xsl:template match="anchor">
  </xsl:template>

  <xsl:template match="pcr-keyword">
    <fo:inline font-weight="bold"><xsl:apply-templates/></fo:inline>
  </xsl:template>

</xsl:stylesheet>
