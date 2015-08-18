<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- writes all fileref attributes found
     to stdout
     see $OLDPCRTREE/docs/pcrmanual/filerefDepend

     TODO: check DocBook DTD wrsl. nog simpeler:
           elk fileref attribuut is een externe depency
  -->
<!DOCTYPE xsl:transform>
<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:xi="http://www.w3.org/2001/XInclude"
 version="1.0"
>

<xsl:output method="text"/>
<xsl:strip-space elements="*" />

<xsl:template match="*">
<xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="graphic">
  <xsl:value-of select="@fileref"/>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<xsl:template match="inlinegraphic">
  <xsl:value-of select="@fileref"/>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<xsl:template match="imagedata">
  <xsl:value-of select="@fileref"/>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<xsl:template match="xi:include">
  <xsl:value-of select="@href"/>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

</xsl:transform>
