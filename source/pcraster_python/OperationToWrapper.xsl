<?xml version="1.0"?>

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  version="1.1"
>
<xsl:output method="text" encoding="UTF-8"/>
<xsl:strip-space elements="*"/>



<xsl:include href="strlib.xsl"/>



<!-- Writes the name of an operation as the user sees it. -->
<xsl:template name="name">
  <xsl:param name="operation"/>
  <xsl:param name="result"/>

  <!-- Operator prefix. -->
  <xsl:if test="$operation/@syntax='Operator'">
    <xsl:text>pcr</xsl:text>
  </xsl:if>

  <xsl:choose>
    <xsl:when test="$operation/@name='if'">
      <xsl:value-of select="$operation/@implName"/>
    </xsl:when>
    <xsl:when test="$operation/@syntax='Operator' and $operation/@implName">
      <xsl:call-template name="replace">
        <xsl:with-param name="string" select="$operation/@implName"/>
        <xsl:with-param name="from" select="'_'"/>
        <xsl:with-param name="to" select="''"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:when test="$operation/@syntax='MRF'">
      <xsl:value-of select="concat($operation/@name, $result/@functionSuffix)"/>
    </xsl:when>
    <xsl:otherwise>
      <!-- General case. -->
      <xsl:value-of select="$operation/@name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- Writes the name of the implementation to be called. -->
<xsl:template name="implementationName">
  <xsl:param name="operation"/>
  <xsl:choose>
    <xsl:when test="$operation/@implName">
      <xsl:value-of select="$operation/@implName"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$operation/@name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- Writes the name of an argument. -->
<xsl:template name="argumentName">
  <xsl:param name="argument"/>
    <xsl:value-of select="concat('arg', position())"/>
</xsl:template>



<!-- Writes an argument list for a Python function. -->
<xsl:template name="pythonArgumentList">
  <xsl:param name="operation"/>

  <xsl:for-each select="$operation/Input">
    <xsl:if test="@repeat='true'">
      <!-- argument can be repeated -->
      <xsl:text>*</xsl:text>
    </xsl:if>
    <xsl:call-template name="argumentName">
      <xsl:with-param name="argument" select="."/>
    </xsl:call-template>
    <xsl:if test="position() != last()">
      <xsl:value-of select="', '"/>
    </xsl:if>
  </xsl:for-each>
</xsl:template>



<!-- Writes an opcode of an operation. -->
<xsl:template name="opcode">
  <xsl:param name="operation"/>
  <xsl:param name="result"/>

  <xsl:variable name="name">
    <xsl:call-template name="implementationName">
      <xsl:with-param name="operation" select="$operation"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:text>OP_</xsl:text>
  <xsl:call-template name="toupper">
    <xsl:with-param name="str" select="$name"/>
  </xsl:call-template>
  <xsl:if test="$operation/@syntax = 'MRF'">
    <xsl:call-template name="toupper">
      <xsl:with-param name="str" select="$result/@functionSuffix"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>



<!-- Writes code to convert arguments of python operation, when needed. -->
<xsl:template name="pythonConvertArguments">
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:for-each select="$operation/Input">
    <xsl:variable name="argumentName" select="concat('arg', position())"/>
    <xsl:choose>
      <xsl:when test="@repeat='true'">
        <!-- Only field arguments can be repeatable. -->
        <!-- Varargs are tuples which can't be mutated: convert to list. -->
        <xsl:value-of select="concat($indentation, '    ', $argumentName, ' = list(', $argumentName, ')&#xa;')"/>
        <xsl:value-of select="concat($indentation, '    for i in range(len(', $argumentName, ')):&#xa;')"/>
        <xsl:value-of select="concat($indentation, '      if isinstance(', $argumentName, '[i], str):&#xa;')"/>
        <xsl:value-of select="concat($indentation, '        ', $argumentName, '[i] = _pcraster.readmap(', $argumentName, '[i])&#xa;')"/>
        <xsl:value-of select="concat($indentation, '      elif isinstance(', $argumentName, '[i], int) or isinstance(', $argumentName, '[i], float):&#xa;')"/>
        <xsl:value-of select="concat($indentation, '        ', $argumentName, '[i] = _pcraster.newNonSpatialField(', $argumentName, '[i])&#xa;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="Field">
            <xsl:value-of select="concat($indentation, '    if isinstance(', $argumentName, ', str):&#xa;')"/>
            <xsl:value-of select="concat($indentation, '      ', $argumentName, ' = _pcraster.readmap(', $argumentName, ')&#xa;')"/>
            <xsl:value-of select="concat($indentation, '    elif isinstance(', $argumentName, ', int) or isinstance(', $argumentName, ', float):&#xa;')"/>
            <xsl:value-of select="concat($indentation, '      ', $argumentName, ' = _pcraster.newNonSpatialField(', $argumentName, ')&#xa;')"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="concat($indentation, '    ', $argumentName, ' = _pcraster.DataStorageId(', $argumentName, ')&#xa;')"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>



<xsl:template name="pythonCreateResultsList">
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:if test="count($operation/Result)">
    <xsl:value-of select="$indentation"/>
    <xsl:text>    results = []&#xA;</xsl:text>
  </xsl:if>
</xsl:template>



<xsl:template name="nrResults">
  <xsl:param name="operation"/>

  <xsl:choose>
    <xsl:when test="$operation/../ObjectLinkMethod">
      <xsl:value-of select="count($operation/Result)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>1</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="popField">
  <xsl:param name="count"/>
  <xsl:param name="iteration">1</xsl:param>
  <xsl:param name="indentation"/>
  <xsl:value-of select="concat($indentation, '    results.append(_pcraster._rte().releasePopField())&#xA;')"/>

  <xsl:if test="$iteration &lt; $count">
    <xsl:call-template name="popField">
      <xsl:with-param name="count" select="$count"/>
      <xsl:with-param name="iteration" select="$iteration+1"/>
      <xsl:with-param name="indentation" select="$indentation"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="pythonCalculateResults">
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:for-each select="$operation/Input">
    <xsl:variable name="argumentName" select="concat('arg', position())"/>
    <xsl:choose>
      <xsl:when test="Field">
        <xsl:value-of select="concat($indentation, '    _pcraster._rte().pushField(', $argumentName, ')&#xa;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat($indentation, '    _pcraster._rte().pushDataStorageId(', $argumentName, ')&#xa;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>

  <xsl:variable name="nrArguments" select="count($operation/Input)"/>
  <xsl:value-of select="concat($indentation, '    _pcraster._rte().checkAndExec(operator, ', $nrArguments, ')&#xA;')"/>


  <xsl:variable name="nrResults">
    <xsl:call-template name="nrResults">
      <xsl:with-param name="operation" select="$operation"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:if test="$nrResults > 0">
    <xsl:call-template name="popField">
      <xsl:with-param name="count" select="$nrResults"/>
      <xsl:with-param name="indentation" select="$indentation"/>
    </xsl:call-template>

<!--
    <xsl:for-each select="saxon:range(1, $nrResults)">
      <xsl:value-of select="concat($indentation, '    results.append(_pcraster._rte().releasePopField())&#xA;')"/>
    </xsl:for-each>
-->

    <xsl:if test="$nrResults > 1">
      <xsl:value-of select="concat($indentation, '    results.reverse()&#xA;')"/>
    </xsl:if>
  </xsl:if>
</xsl:template>



<xsl:template name="pythonConvertAndReturnResults">
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:variable name="nrResults">
    <xsl:call-template name="nrResults">
      <xsl:with-param name="operation" select="$operation"/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:if test="$nrResults > 0">
    <xsl:value-of select="$indentation"/>
    <xsl:text>    return</xsl:text>
    <xsl:choose>
      <xsl:when test="$nrResults = 1">
        <xsl:text> results[0]</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:choose>
          <xsl:when test="$operation/../Operation">
            <xsl:text> results[</xsl:text>
            <!--
              Position is determined by for-each loop in
              pythonInternalOperation! Tricky, prob. be better to make it a
              template parameter.
            -->
            <xsl:value-of select="position() - 1"/>
            <xsl:text>]</xsl:text>
          </xsl:when>
          <xsl:otherwise>
            <xsl:text> results</xsl:text>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>&#xA;</xsl:text>
  </xsl:if>
</xsl:template>




<!-- Used by pythonOperation template. -->
<xsl:template name="pythonInternalOperation">
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:for-each select="$operation/Result">

    <xsl:variable name="result" select="."/>

    <!-- Determine name of operation as the user sees it. -->
    <xsl:variable name="operationName">
      <xsl:call-template name="name">
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="result" select="$result"/>
      </xsl:call-template>
    </xsl:variable>

    <!-- Create function prototype. -->
    <xsl:value-of select="concat($indentation, 'def ', $operationName, '(')"/>
    <xsl:call-template name="pythonArgumentList">
      <xsl:with-param name="operation" select="$operation"/>
    </xsl:call-template>
    <xsl:text>):&#xa;</xsl:text>

    <xsl:choose>
      <xsl:when test="count($operation/Input[not(Field) and not(Table) and not(TimeSeries) and not(MapStack)])">
        <xsl:value-of select="$indentation"/>
        <xsl:value-of select="concat('  raise RuntimeError(&quot;The ', $operationName, ' operation is not implemented. Only operations with map, table or timeseries arguments are currenly supported&quot;)&#xA;')"/>
      </xsl:when>
      <xsl:when test="count($operation/Result[not(Field)])">
        <xsl:value-of select="$indentation"/>
        <xsl:value-of select="concat('  raise RuntimeError(&quot;The ', $operationName, ' operation is not implemented. Only operations with map results are currenly supported&quot;)&#xA;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat($indentation, '  try:&#xA;')"/>
        <xsl:call-template name="pythonConvertArguments">
          <xsl:with-param name="operation" select="$operation"/>
          <xsl:with-param name="indentation" select="$indentation"/>
        </xsl:call-template>

        <!-- Create operator. -->
        <xsl:variable name="opcode">
          <xsl:call-template name="opcode">
            <xsl:with-param name="operation" select="$operation"/>
            <xsl:with-param name="result" select="$result"/>
          </xsl:call-template>
        </xsl:variable>
        <xsl:value-of select="concat($indentation, '    operator = _pcraster._major2op(_pcraster.MAJOR_CODE.', $opcode, ')&#xA;')"/>

        <xsl:call-template name="pythonCreateResultsList">
          <xsl:with-param name="operation" select="$operation"/>
          <xsl:with-param name="indentation" select="$indentation"/>
        </xsl:call-template>

        <xsl:variable name="nrFieldArguments" select="count($operation/Input/Field)"/>

        <xsl:choose>
          <xsl:when test="$operation/Input[@repeat='true']">
            <xsl:variable name="varArg" select="$operation/Input[@repeat='true']"/>
            <xsl:variable name="position" select="count($varArg/preceding-sibling::Input) + 1"/>
            <xsl:variable name="varArgName" select="concat('arg', $position)"/>
            <xsl:variable name="argumentName" select="concat('arg', $position - 1)"/>
            <xsl:choose>
              <xsl:when test="not($operation/Input[not(Field)])">
                <xsl:choose>
                  <xsl:when test="$position = 1">
                    <!-- Vararg is first and only argument. -->
                    <xsl:value-of select="concat($indentation, '    for i in range(len(', $varArgName, ')):&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '      _pcraster._rte().pushField(', $varArgName, '[i])&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '    _pcraster._rte().checkAndExec(operator, len(', $varArgName, '))&#xA;')"/>
                    <xsl:value-of select="concat($indentation, '    results.append(_pcraster._rte().releasePopField())&#xa;')"/>
                  </xsl:when>
                  <xsl:otherwise>
                    <!-- Vararg is last argument. -->
                    <xsl:value-of select="concat($indentation, '    for i in range(len(', $varArgName, ')):&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '      _pcraster._rte().pushField(', $argumentName, ')&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '      _pcraster._rte().pushField(', $varArgName, '[i])&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '      _pcraster._rte().checkAndExec(operator, ', $nrFieldArguments, ')&#xA;')"/>
                    <xsl:value-of select="concat($indentation, '      ', $argumentName, ' = _pcraster._rte().releasePopField()&#xa;')"/>
                    <xsl:value-of select="concat($indentation, '    results.append(', $argumentName, ')&#xA;')"/>
                  </xsl:otherwise>
                </xsl:choose>
              </xsl:when>
              <xsl:otherwise>
                <xsl:for-each select="$operation/Input">
                  <xsl:if test="position() &lt; $position">
                    <xsl:choose>
                      <xsl:when test="Field">
                        <xsl:value-of select="concat($indentation, '    _pcraster._rte().pushField(', $argumentName, ')&#xa;')"/>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:value-of select="concat($indentation, '    _pcraster._rte().pushDataStorageId(', $argumentName, ')&#xa;')"/>
                      </xsl:otherwise>
                    </xsl:choose>
                  </xsl:if>
                </xsl:for-each>
                <xsl:value-of select="concat($indentation, '    for i in range(len(', $varArgName, ')):&#xa;')"/>
                <xsl:value-of select="concat($indentation, '      _pcraster._rte().pushField(', $varArgName, '[i])&#xa;')"/>
                <xsl:value-of select="concat($indentation, '    _pcraster._rte().checkAndExec(operator, ', $position - 1, ' + len(', $varArgName, '))&#xA;')"/>
                <xsl:value-of select="concat($indentation, '    results.append(_pcraster._rte().releasePopField())&#xa;')"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="pythonCalculateResults">
              <xsl:with-param name="operation" select="$operation"/>
              <xsl:with-param name="indentation" select="$indentation"/>
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>

        <xsl:call-template name="pythonConvertAndReturnResults">
          <xsl:with-param name="operation" select="$operation"/>
          <xsl:with-param name="indentation" select="$indentation"/>
        </xsl:call-template>

        <xsl:value-of select="$indentation"/>
        <xsl:text>  except RuntimeError as exception:&#xA;</xsl:text>
        <xsl:value-of select="concat($indentation, '    raise RuntimeError(&quot;', $operationName, ': %s&quot; % (str(exception)))&#xA;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:for-each>
</xsl:template>



<xsl:template name="pythonObjectLinkOperation">
  <xsl:param name="className"/>
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <!-- Determine name of operation as the user sees it. -->
  <xsl:variable name="operationName">
    <xsl:call-template name="name">
      <xsl:with-param name="operation" select="$operation"/>
    </xsl:call-template>
  </xsl:variable>

  <!-- Create function prototype. -->
  <xsl:value-of select="concat($indentation, 'def ', $operationName, '(self')"/>
  <xsl:if test="$operation/Input">
    <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:call-template name="pythonArgumentList">
    <xsl:with-param name="operation" select="$operation"/>
  </xsl:call-template>
  <xsl:text>):&#xa;</xsl:text>

  <xsl:choose>
    <xsl:when test="count($operation/Input[not(Field)])">
      <xsl:value-of select="$indentation"/>
        <xsl:value-of select="concat('  raise RuntimeError(&quot;The ', $operationName, ' operation is not implemented. Only operations with field arguments are currenly supported&quot;)&#xA;')"/>
    </xsl:when>
    <xsl:when test="count($operation/Result[not(Field)])">
      <xsl:value-of select="$indentation"/>
        <xsl:value-of select="concat('  raise RuntimeError(&quot;The ', $operationName, ' operation is not implemented. Only operations with field results are currenly supported&quot;)&#xA;')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="concat($indentation, '  try:&#xA;')"/>
      <xsl:call-template name="pythonConvertArguments">
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="indentation" select="$indentation"/>
      </xsl:call-template>

      <!-- Create operator. -->
      <xsl:value-of select="concat($indentation, '    operator = _pcraster._opName2op(&quot;', $className, '::', $operation/@name, '&quot;)&#xA;')"/>

      <xsl:call-template name="pythonCreateResultsList">
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="indentation" select="$indentation"/>
      </xsl:call-template>

      <xsl:variable name="nrArguments" select="count($operation/Input/Field)"/>
      <xsl:choose>
        <xsl:when test="$operation/Input[@repeat='true']">
        <!--
          Not supported yet, but see above for implementation.
          -->
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="concat($indentation, '    _pcraster._rte().pushObjectLink(self.d_objectLink)&#xa;')"/>
          <xsl:call-template name="pythonCalculateResults">
            <xsl:with-param name="operation" select="$operation"/>
            <xsl:with-param name="indentation" select="$indentation"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>

      <xsl:call-template name="pythonConvertAndReturnResults">
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="indentation" select="$indentation"/>
      </xsl:call-template>

      <xsl:value-of select="$indentation"/>
      <xsl:text>  except RuntimeError as exception:&#xA;</xsl:text>
      <xsl:value-of select="concat($indentation, '    raise RuntimeError(&quot;', $className, '::', $operationName, ': %s&quot; % (str(exception)))&#xA;')"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<!-- Writes an Python implementation of an operation. -->
<!-- Toplevel template. -->
<xsl:template name="pythonOperation">
  <xsl:param name="className"/>
  <xsl:param name="operation"/>
  <xsl:param name="indentation"/>

  <xsl:choose>
    <xsl:when test="$operation/../Operation">
      <xsl:call-template name="pythonInternalOperation">
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="indentation" select="$indentation"/>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise> <!-- <xsl:if test="$operation/../ObjectLinkMethod"> -->
      <xsl:call-template name="pythonObjectLinkOperation">
        <xsl:with-param name="className" select="$className"/>
        <xsl:with-param name="operation" select="$operation"/>
        <xsl:with-param name="indentation" select="$indentation"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>

