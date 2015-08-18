<xsl:stylesheet
 version="1.1"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:saxon="http://icl.com/saxon"
 xmlns:StringBuffer="java:java.lang.StringBuffer"
 exclude-result-prefixes="saxon StringBuffer">

<xsl:template name="showMessage">
  <xsl:param name="terminate"/>
  <xsl:param name="message"/>
  <xsl:message terminate="$terminate">
    <xsl:value-of select="$message"/>
  </xsl:message>
</xsl:template>

<xsl:template name="showInfo">
  <xsl:param name="message"/>
  <xsl:call-template name="showMessage">
    <xsl:with-param name="message" select="concat('Info   : ', $message)"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="showWarning">
  <xsl:param name="message"/>
  <xsl:call-template name="showMessage">
    <xsl:with-param name="message" select="concat('Warning: ', $message)"/>
  </xsl:call-template>
</xsl:template>

<xsl:template name="showError">
  <xsl:param name="terminate"/>
  <xsl:param name="message"/>
  <xsl:call-template name="showMessage">
    <xsl:with-param name="terminate" select="$terminate"/>
    <xsl:with-param name="message" select="concat('Error  : ', $message)"/>
  </xsl:call-template>
</xsl:template>

<!-- Search and replace in string. -->
<xsl:template name="replace">
  <xsl:param name="string"/>
  <xsl:param name="from"/>
  <xsl:param name="to"/>
  <xsl:choose>
    <xsl:when test="contains($string, $from)">
      <xsl:value-of select="substring-before($string, $from)"/>
      <xsl:value-of disable-output-escaping="yes" select="$to"/>
      <xsl:call-template name="replace">
        <xsl:with-param name="string" select="substring-after($string, $from)"/>
        <xsl:with-param name="from" select="$from"/>
        <xsl:with-param name="to" select="$to"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$string"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="toupper">
  <xsl:param name="str"/>

  <xsl:value-of select="translate($str,
         'abcdefghijklmnopqrstuvwxyz',
         'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
</xsl:template>

<!-- Depricated -->
<xsl:template name="pcr-toupper">
  <xsl:param name="str"/>

  <xsl:message>
    <xsl:text>pcr-toupper is depricated, use upper template</xsl:text>
  </xsl:message>

  <xsl:value-of select="translate($str,
         'abcdefghijklmnopqrstuvwxyz',
         'ABCDEFGHIJKLMNOPQRSTUVWXYZ')"/>
</xsl:template>

<!-- Strips whitespace characters from beginning and end of string. -->
<xsl:template name="strip">
  <xsl:param name="str"/>

  <!-- Remove whitespace from left. -->
  <xsl:variable name="lstripped">
    <xsl:call-template name="lstrip">
      <xsl:with-param name="str" select="$str"/>
    </xsl:call-template>
  </xsl:variable>

  <!-- Remove whitespace from right. -->
  <xsl:variable name="stripped">
    <xsl:call-template name="rstrip">
      <xsl:with-param name="str" select="$lstripped"/>
    </xsl:call-template>
  </xsl:variable>

  <!-- Output stripped value. -->
  <xsl:value-of select="$stripped"/>
</xsl:template>


<!-- Strips whitespace from the left of the string. -->
<xsl:template name="lstrip">
  <xsl:param name="str"/>

  <xsl:choose>
    <xsl:when test="starts-with($str, '&#xA;') or
                    starts-with($str, '&#xD;') or
                    starts-with($str, '&#x20;') or
                    starts-with($str, '&#x9;')">
      <xsl:call-template name="lstrip">
        <xsl:with-param name="str" select="substring($str, 2)"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$str"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- Strips whitespace from the right of the string. -->
<xsl:template name="rstrip">
  <xsl:param name="str"/>

  <xsl:variable name="last-char"
    select="substring($str, string-length($str), 1)"/>
  <xsl:choose>
    <xsl:when test="($last-char = '&#xA;') or
                    ($last-char = '&#xD;') or
                    ($last-char = '&#x20;') or
                    ($last-char = '&#x9;')">
      <xsl:call-template name="rstrip">
        <xsl:with-param name="str"
          select="substring($str, 1, string-length($str) - 1)"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$str"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="escapeRegexCharacters">
  <xsl:param name="regex"/>

  <!-- First, escape the escape character itself!!! -->
  <xsl:variable name="v1">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$regex"/>
      <xsl:with-param name="from" select="'\'"/>
      <xsl:with-param name="to" select="'\\'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v2">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v1"/>
      <xsl:with-param name="from" select="'.'"/>
      <xsl:with-param name="to" select="'\.'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v3">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v2"/>
      <xsl:with-param name="from" select="'['"/>
      <xsl:with-param name="to" select="'\['"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v4">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v3"/>
      <xsl:with-param name="from" select="'('"/>
      <xsl:with-param name="to" select="'\('"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v5">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v4"/>
      <xsl:with-param name="from" select="')'"/>
      <xsl:with-param name="to" select="'\)'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v6">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v5"/>
      <xsl:with-param name="from" select="'*'"/>
      <xsl:with-param name="to" select="'\*'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v7">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v6"/>
      <xsl:with-param name="from" select="'+'"/>
      <xsl:with-param name="to" select="'\+'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v8">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v7"/>
      <xsl:with-param name="from" select="'?'"/>
      <xsl:with-param name="to" select="'\?'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v9">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v8"/>
      <xsl:with-param name="from" select="'{'"/>
      <xsl:with-param name="to" select="'\{'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v10">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v9"/>
      <xsl:with-param name="from" select="'|'"/>
      <xsl:with-param name="to" select="'\|'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v11">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v10"/>
      <xsl:with-param name="from" select="'^'"/>
      <xsl:with-param name="to" select="'\^'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="v12">
    <xsl:call-template name="replace">
      <xsl:with-param name="string" select="$v11"/>
      <xsl:with-param name="from" select="'$'"/>
      <xsl:with-param name="to" select="'\$'"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:value-of select="$v12"/>
</xsl:template>

<!-- Reverses the characters in a string. -->
<xsl:template name="reverse">
  <xsl:param name="string"/>
  <xsl:value-of select="StringBuffer:reverse(StringBuffer:new($string))"/>
</xsl:template>

</xsl:stylesheet>
