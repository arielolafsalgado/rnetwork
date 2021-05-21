<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>CRAN - Package tableschema.r</title>
<link rel="stylesheet" type="text/css" href="../../CRAN_web.css" />
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="citation_title" content="Table Schema 'Frictionless Data' [R package tableschema.r version 1.1.1]" />
<meta name="citation_author1" content="Kleanthis Koupidis" />
<meta name="citation_author2" content="Lazaros Ioannidis" />
<meta name="citation_author3" content="Charalampos Bratsas" />
<meta name="citation_publication_date" content="2020-03-12" />
<meta name="citation_public_url" content="https://CRAN.R-project.org/package=tableschema.r" />
<meta name="DC.identifier" content="https://CRAN.R-project.org/package=tableschema.r" />
<meta name="DC.publisher" content="Comprehensive R Archive Network (CRAN)" />
<meta name="og:title" content="tableschema.r: Table Schema 'Frictionless Data'" />
<meta name="og:description" content="Allows to work with 'Table Schema' (&amp;lt;&lt;a href=&quot;http://specs.frictionlessdata.io/table-schema/&quot;&gt;http://specs.frictionlessdata.io/table-schema/&lt;/a&gt;&amp;gt;). 'Table Schema' is well suited for use cases around handling and validating tabular data in text formats such as 'csv', but its utility extends well beyond this core usage, towards a range of applications where data benefits from a portable schema format. The 'tableschema.r' package can load and validate any table schema descriptor, allow the creation and modification of descriptors, expose methods for reading and streaming data that conforms to a 'Table Schema' via the 'Tabular Data Resource' abstraction." />
<meta name="og:image" content="https://CRAN.R-project.org/CRANlogo.png" />
<meta name="og:type" content="website" />
<meta name="og:url" content="https://CRAN.R-project.org/package=tableschema.r" />
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@_R_Foundation" />
<style type="text/css">
  table td { vertical-align: top; }
</style>
</head>
<body>
<h2>tableschema.r: Table Schema 'Frictionless Data'</h2>
<p>Allows to work with 'Table Schema' (&lt;<a href="http://specs.frictionlessdata.io/table-schema/">http://specs.frictionlessdata.io/table-schema/</a>&gt;). 'Table Schema' is well suited for use cases around handling and validating tabular data in text formats such as 'csv', but its utility extends well beyond this core usage, towards a range of applications where data benefits from a portable schema format. The 'tableschema.r' package can load and validate any table schema descriptor, allow the creation and modification of descriptors, expose methods for reading and streaming data that conforms to a 'Table Schema' via the 'Tabular Data Resource' abstraction.</p>
<table summary="Package tableschema.r summary">
<tr>
<td>Version:</td>
<td>1.1.1</td>
</tr>
<tr>
<td>Imports:</td>
<td><a href="../config/index.html">config</a>, <a href="../future/index.html">future</a>, <a href="../httr/index.html">httr</a>, <a href="../iterators/index.html">iterators</a>, <a href="../jsonlite/index.html">jsonlite</a>, <a href="../jsonvalidate/index.html">jsonvalidate</a>, <a href="../lubridate/index.html">lubridate</a>, <a href="../purrr/index.html">purrr</a>, <a href="../R6/index.html">R6</a>, <a href="../RCurl/index.html">RCurl</a>, <a href="../rlist/index.html">rlist</a>, <a href="../stringr/index.html">stringr</a>, <a href="../urltools/index.html">urltools</a></td>
</tr>
<tr>
<td>Suggests:</td>
<td><a href="../covr/index.html">covr</a>, <a href="../foreach/index.html">foreach</a>, <a href="../testthat/index.html">testthat</a></td>
</tr>
<tr>
<td>Published:</td>
<td>2020-03-12</td>
</tr>
<tr>
<td>Author:</td>
<td>Kleanthis Koupidis [aut, cre],
  Lazaros Ioannidis [aut],
  Charalampos Bratsas [aut],
  Open Knowledge International [cph]</td>
</tr>
<tr>
<td>Maintainer:</td>
<td>Kleanthis Koupidis  &#x3c;&#x6b;&#x6f;&#x75;&#x70;&#x69;&#x64;&#x69;&#x73;&#x20;&#x61;&#x74;&#x20;&#x6f;&#x6b;&#x66;&#x6e;&#x2e;&#x67;&#x72;&#x3e;</td>
</tr>
<tr>
<td>BugReports:</td>
<td><a href="https://github.com/frictionlessdata/tableschema-r/issues">https://github.com/frictionlessdata/tableschema-r/issues</a></td>
</tr>
<tr>
<td>License:</td>
<td><a href="../../licenses/MIT">MIT</a> + file <a href="LICENSE">LICENSE</a></td>
</tr>
<tr>
<td>URL:</td>
<td><a href="https://github.com/frictionlessdata/tableschema-r">https://github.com/frictionlessdata/tableschema-r</a></td>
</tr>
<tr>
<td>NeedsCompilation:</td>
<td>no</td>
</tr>
<tr>
<td>Materials:</td>
<td><a href="readme/README.html">README</a> <a href="news/news.html">NEWS</a> </td>
</tr>
<tr>
<td>CRAN&nbsp;checks:</td>
<td><a href="../../checks/check_results_tableschema.r.html">tableschema.r results</a></td>
</tr>
</table>
<h4>Downloads:</h4>
<table summary="Package tableschema.r downloads">
<tr>
<td> Reference&nbsp;manual: </td>
<td> <a href="tableschema.r.pdf"> tableschema.r.pdf </a> </td>
</tr>
<tr>
<td> Package&nbsp;source: </td>
<td> <a href="../../../src/contrib/tableschema.r_1.1.1.tar.gz"> tableschema.r_1.1.1.tar.gz </a> </td>
</tr>
<tr>
<td> Windows&nbsp;binaries: </td>
<td> r-devel: <a href="../../../bin/windows/contrib/4.1/tableschema.r_1.1.1.zip">tableschema.r_1.1.1.zip</a>, r-release: <a href="../../../bin/windows/contrib/4.0/tableschema.r_1.1.1.zip">tableschema.r_1.1.1.zip</a>, r-oldrel: <a href="../../../bin/windows/contrib/3.6/tableschema.r_1.1.1.zip">tableschema.r_1.1.1.zip</a> </td>
</tr>
<tr>
<td> macOS&nbsp;binaries: </td>
<td> r-release: <a href="../../../bin/macosx/contrib/4.0/tableschema.r_1.1.1.tgz">tableschema.r_1.1.1.tgz</a>, r-oldrel: <a href="../../../bin/macosx/el-capitan/contrib/3.6/tableschema.r_1.1.1.tgz">tableschema.r_1.1.1.tgz</a> </td>
</tr>
<tr>
<td> Old&nbsp;sources: </td>
<td> <a href="https://CRAN.R-project.org/src/contrib/Archive/tableschema.r"> tableschema.r archive </a> </td>
</tr>
</table>
<h4>Reverse dependencies:</h4>
<table summary="Package tableschema.r reverse dependencies">
<tr>
<td>Reverse&nbsp;imports:</td>
<td><a href="../datapackage.r/index.html">datapackage.r</a></td>
</tr>
</table>
<h4>Linking:</h4>
<p>Please use the canonical form
<a href="https://CRAN.R-project.org/package=tableschema.r"><samp>https://CRAN.R-project.org/package=tableschema.r</samp></a>
to link to this page.</p>
</body>
</html>
