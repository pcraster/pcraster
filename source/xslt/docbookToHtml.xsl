<?xml version='1.0'?>
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.0">

  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl"/>
  <xsl:include href="docbookCommon.xsl"/>

  <xsl:template match="pcr-op">
    <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="br">
    <br/>
  </xsl:template>

  <xsl:template match="anchor">
  </xsl:template>

  <xsl:template match="pcr-keyword">
    <b><xsl:apply-templates/></b>
  </xsl:template>

</xsl:stylesheet>