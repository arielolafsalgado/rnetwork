<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>CRAN - Package bea.R</title>
<link rel="stylesheet" type="text/css" href="../../CRAN_web.css" />
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="citation_title" content="Bureau of Economic Analysis API [R package bea.R version 1.0.6]" />
<meta name="citation_author" content="Andrea Batch" />
<meta name="citation_publication_date" content="2018-02-23" />
<meta name="citation_public_url" content="https://CRAN.R-project.org/package=bea.R" />
<meta name="DC.identifier" content="https://CRAN.R-project.org/package=bea.R" />
<meta name="DC.publisher" content="Comprehensive R Archive Network (CRAN)" />
<meta name="og:title" content="bea.R: Bureau of Economic Analysis API" />
<meta name="og:description" content="Provides an R interface for the Bureau of Economic Analysis (BEA) API (see &amp;lt;&lt;a href=&quot;http://www.bea.gov/API/bea_web_service_api_user_guide.htm&quot;&gt;http://www.bea.gov/API/bea_web_service_api_user_guide.htm&lt;/a&gt;&amp;gt; for more information) that serves two core purposes - 1. To Extract/Transform/Load data [beaGet()] from the BEA API as R-friendly formats in the user's work space [transformation done by default in beaGet() can be modified using optional parameters; see, too, bea2List(), bea2Tab()]. 2. To enable the search of descriptive meta data [beaSearch()]. Other features of the library exist mainly as intermediate methods or are in early stages of development. Important Note - You must have an API key to use this library. Register for a key at &amp;lt;&lt;a href=&quot;http://www.bea.gov/API/signup/index.cfm&quot;&gt;http://www.bea.gov/API/signup/index.cfm&lt;/a&gt;&amp;gt; ." />
<meta name="og:image" content="https://CRAN.R-project.org/CRANlogo.png" />
<meta name="og:type" content="website" />
<meta name="og:url" content="https://CRAN.R-project.org/package=bea.R" />
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@_R_Foundation" />
<style type="text/css">
  table td { vertical-align: top; }
</style>
</head>
<body>
<h2>bea.R: Bureau of Economic Analysis API</h2>
<p>Provides an R interface for the Bureau of Economic Analysis (BEA) 
		API (see &lt;<a href="http://www.bea.gov/API/bea_web_service_api_user_guide.htm">http://www.bea.gov/API/bea_web_service_api_user_guide.htm</a>&gt; for 
		more information) that serves two core purposes - 
    1. To Extract/Transform/Load data [beaGet()] from the BEA API as R-friendly 
		formats in the user's work space [transformation done by default in beaGet() 
		can be modified using optional parameters; see, too, bea2List(), bea2Tab()].
		2. To enable the search of descriptive meta data [beaSearch()].
		Other features of the library exist mainly as intermediate methods 
		or are in early stages of development.
		Important Note - You must have an API key to use this library.  
		Register for a key at &lt;<a href="http://www.bea.gov/API/signup/index.cfm">http://www.bea.gov/API/signup/index.cfm</a>&gt; .</p>
<table summary="Package bea.R summary">
<tr>
<td>Version:</td>
<td>1.0.6</td>
</tr>
<tr>
<td>Depends:</td>
<td>R (&ge; 3.2.1), <a href="../data.table/index.html">data.table</a></td>
</tr>
<tr>
<td>Imports:</td>
<td><a href="../httr/index.html">httr</a>, <a href="../DT/index.html">DT</a>, <a href="../shiny/index.html">shiny</a>, <a href="../jsonlite/index.html">jsonlite</a>, <a href="../googleVis/index.html">googleVis</a>, <a href="../shinydashboard/index.html">shinydashboard</a>, <a href="../ggplot2/index.html">ggplot2</a>, <a href="../stringr/index.html">stringr</a>, <a href="../chron/index.html">chron</a>, <a href="../gtable/index.html">gtable</a>, <a href="../scales/index.html">scales</a>, <a href="../htmltools/index.html">htmltools</a>, <a href="../httpuv/index.html">httpuv</a>, <a href="../xtable/index.html">xtable</a>, <a href="../stringi/index.html">stringi</a>, <a href="../magrittr/index.html">magrittr</a>, <a href="../htmlwidgets/index.html">htmlwidgets</a>, <a href="../Rcpp/index.html">Rcpp</a>, <a href="../munsell/index.html">munsell</a>, <a href="../colorspace/index.html">colorspace</a>, <a href="../plyr/index.html">plyr</a>, <a href="../yaml/index.html">yaml</a></td>
</tr>
<tr>
<td>Published:</td>
<td>2018-02-23</td>
</tr>
<tr>
<td>Author:</td>
<td>Andrea Batch [aut, cre],
  Jeff Chen [ctb],
  Walt Kampas [ctb]</td>
</tr>
<tr>
<td>Maintainer:</td>
<td>Andrea Batch  &#x3c;&#x41;&#x6e;&#x64;&#x72;&#x65;&#x61;&#x2e;&#x4a;&#x75;&#x6c;&#x63;&#x61;&#x20;&#x61;&#x74;&#x20;&#x62;&#x65;&#x61;&#x2e;&#x67;&#x6f;&#x76;&#x3e;</td>
</tr>
<tr>
<td>License:</td>
<td><a href="https://creativecommons.org/publicdomain/zero/1.0/legalcode">CC0</a></td>
</tr>
<tr>
<td>URL:</td>
<td><a href="https://github.com/us-bea/bea.R">https://github.com/us-bea/bea.R</a></td>
</tr>
<tr>
<td>NeedsCompilation:</td>
<td>no</td>
</tr>
<tr>
<td>CRAN&nbsp;checks:</td>
<td><a href="../../checks/check_results_bea.R.html">bea.R results</a></td>
</tr>
</table>
<h4>Downloads:</h4>
<table summary="Package bea.R downloads">
<tr>
<td> Reference&nbsp;manual: </td>
<td> <a href="bea.R.pdf"> bea.R.pdf </a> </td>
</tr>
<tr>
<td> Package&nbsp;source: </td>
<td> <a href="../../../src/contrib/bea.R_1.0.6.tar.gz"> bea.R_1.0.6.tar.gz </a> </td>
</tr>
<tr>
<td> Windows&nbsp;binaries: </td>
<td> r-devel: <a href="../../../bin/windows/contrib/4.1/bea.R_1.0.6.zip">bea.R_1.0.6.zip</a>, r-release: <a href="../../../bin/windows/contrib/4.0/bea.R_1.0.6.zip">bea.R_1.0.6.zip</a>, r-oldrel: <a href="../../../bin/windows/contrib/3.6/bea.R_1.0.6.zip">bea.R_1.0.6.zip</a> </td>
</tr>
<tr>
<td> macOS&nbsp;binaries: </td>
<td> r-release: <a href="../../../bin/macosx/contrib/4.0/bea.R_1.0.6.tgz">bea.R_1.0.6.tgz</a>, r-oldrel: <a href="../../../bin/macosx/el-capitan/contrib/3.6/bea.R_1.0.6.tgz">bea.R_1.0.6.tgz</a> </td>
</tr>
<tr>
<td> Old&nbsp;sources: </td>
<td> <a href="https://CRAN.R-project.org/src/contrib/Archive/bea.R"> bea.R archive </a> </td>
</tr>
</table>
<h4>Linking:</h4>
<p>Please use the canonical form
<a href="https://CRAN.R-project.org/package=bea.R"><samp>https://CRAN.R-project.org/package=bea.R</samp></a>
to link to this page.</p>
</body>
</html>
