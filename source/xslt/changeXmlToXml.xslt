<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>
<!--
    boilerplate ($DEVENV/Templates/xslt/changeXmlToXml.xsl)
    for doing selective change on XML.
    defaultTemplate at bottom just copies input to output.
    Writing more specific templates can implement
    changes. Some template recipes are included.
    recipes includes:
      oldName2newName
      stringEnumToElementName
  -->

<xsl:output method="xml" indent="yes"/>

<!--
      stringEnumToElementName
      Change:
       <stringEnumElementName>EnumValue</stringEnumElementName>
      To:
       <stringEnumElementName><EnumValue/></stringEnumElementName>
 -->
<xsl:template match="stringEnumElementName">
 <stringEnumElementName>
  <xsl:element name="{string()}"/>
 </stringEnumElementName>
</xsl:template>

<!--
    oldName2NewName
     change an element name
     retain attributes
 -->
<xsl:template match="oldName">
  <newName>
   <!-- copy input attributes -->
   <xsl:apply-templates select="@*"/>
   <!-- recurse into child elements -->
   <xsl:apply-templates/>
  </newName>
</xsl:template>

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
