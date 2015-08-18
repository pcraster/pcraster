<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<!--
    boilerplate ($DEVENV/Templates/xslt/changeXmlToText.xsl)
    for dumping XML to a text form.
    defaultTemplate at bottom just applies all recursively.
    Writing more specific templates can implement
    behavior.
  -->

<xsl:output method="text"/>

<!-- defaultTemplate
     doing a deep apply
     see  xsl-copy in Kay 2nd edition
     All recipes also have to follow this
     pattern.
  -->
<xsl:template match="@*|node()">
 <xsl:copy>
   <!-- copy input attributes -->
  <xsl:apply-templates select="@*"/>
   <!-- recurse into child elements -->
  <xsl:apply-templates />
 </xsl:copy>
</xsl:template>

<!-- somehow this needed "sometimes"
     otherwise above defaultTemplate
     result in 2 nested root element
 -->
<xsl:template match="/">
 <xsl:copy>
   <!-- copy input attributes -->
  <xsl:apply-templates select="@*"/>
   <!-- recurse into child elements -->
  <xsl:apply-templates />
 </xsl:copy>
</xsl:template>

</xsl:transform>
