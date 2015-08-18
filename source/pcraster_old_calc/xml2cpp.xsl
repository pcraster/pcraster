<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<xsl:output method="text"/>
<xsl:strip-space elements="*" />

<!-- \sa
Shortening XSLT Stylesheets
   http://wwwa.xml.com/pub/a/2003/06/11/short-xslt.html
-->

<!-- function list -->
<xsl:template match="Operation[@syntax='Function']">
  <xsl:value-of select="@name"/>
  <xsl:text>&#xa;</xsl:text>
</xsl:template>

<xsl:template match="Operation[@exec='DOUBLE']">
    <xsl:for-each select="Result">
      <xsl:value-of select="parent::Operation/attribute::name"/>
      <xsl:value-of select="@resultName"/>
      <xsl:text>&#xa;</xsl:text>
    </xsl:for-each>
</xsl:template>

</xsl:transform>
