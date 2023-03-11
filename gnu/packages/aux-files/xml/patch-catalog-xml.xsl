<?xml version="1.0" encoding="UTF-8"?>
<!--
SPDX-FileCopyrightText: 2023 Bruno Victal <mirai@makinata.eu>
SPDX-License-Identifier: ISC

Fix uri attributes to point to paths in the store.
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml"/>
  <xsl:param name="prefix">/</xsl:param>
  <!-- begin identity transform -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  <!-- end identity transform -->

  <xsl:template match="@uri">
    <xsl:attribute name="uri">
      <xsl:value-of select="concat('file://', $prefix, '/', .)"/>
    </xsl:attribute>
  </xsl:template>
</xsl:stylesheet>
