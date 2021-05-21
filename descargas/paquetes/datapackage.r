<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>CRAN - Package datapackage.r</title>
<link rel="stylesheet" type="text/css" href="../../CRAN_web.css" />
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="citation_title" content="Data Package 'Frictionless Data' [R package datapackage.r version 1.3.1]" />
<meta name="citation_author1" content="Kleanthis Koupidis" />
<meta name="citation_author2" content="Lazaros Ioannidis" />
<meta name="citation_author3" content="Charalampos Bratsas" />
<meta name="citation_publication_date" content="2020-05-06" />
<meta name="citation_public_url" content="https://CRAN.R-project.org/package=datapackage.r" />
<meta name="DC.identifier" content="https://CRAN.R-project.org/package=datapackage.r" />
<meta name="DC.publisher" content="Comprehensive R Archive Network (CRAN)" />
<meta name="og:title" content="datapackage.r: Data Package 'Frictionless Data'" />
<meta name="og:description" content="Work with 'Frictionless Data Packages' (&amp;lt;&lt;a href=&quot;https://frictionlessdata.io/specs/data-package/&quot;&gt;https://frictionlessdata.io/specs/data-package/&lt;/a&gt;&amp;gt;). Allows to load and validate any descriptor for a data package profile, create and modify descriptors and provides expose methods for reading and streaming data in the package. When a descriptor is a 'Tabular Data Package', it uses the 'Table Schema' package (&amp;lt;&lt;a href=&quot;https://CRAN.R-project.org/package=tableschema.r&quot;&gt;https://CRAN.R-project.org/package=tableschema.r&lt;/a&gt;&amp;gt;) and exposes its functionality, for each resource object in the resources field." />
<meta name="og:image" content="https://CRAN.R-project.org/CRANlogo.png" />
<meta name="og:type" content="website" />
<meta name="og:url" content="https://CRAN.R-project.org/package=datapackage.r" />
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@_R_Foundation" />
<style type="text/css">
  table td { vertical-align: top; }
</style>
</head>
<body>
<h2>datapackage.r: Data Package 'Frictionless Data'</h2>
<p>Work with 'Frictionless Data Packages' (&lt;<a href="https://frictionlessdata.io/specs/data-package/">https://frictionlessdata.io/specs/data-package/</a>&gt;). Allows to load and validate any descriptor for a data package profile, create and modify descriptors and provides expose methods for reading and streaming data in the package. When a descriptor is a 'Tabular Data Package', it uses the 'Table Schema' package (&lt;<a href="https://CRAN.R-project.org/package=tableschema.r">https://CRAN.R-project.org/package=tableschema.r</a>&gt;) and exposes its functionality, for each resource object in the resources field.</p>
<table summary="Package datapackage.r summary">
<tr>
<td>Version:</td>
<td>1.3.1</td>
</tr>
<tr>
<td>Imports:</td>
<td><a href="../config/index.html">config</a>, <a href="../future/index.html">future</a>, <a href="../httr/index.html">httr</a>, <a href="../iterators/index.html">iterators</a>, <a href="../jsonlite/index.html">jsonlite</a>, <a href="../jsonvalidate/index.html">jsonvalidate</a>, <a href="../purrr/index.html">purrr</a>, <a href="../R6/index.html">R6</a>, <a href="../R.utils/index.html">R.utils</a>, <a href="../readr/index.html">readr</a>, <a href="../rlist/index.html">rlist</a>, <a href="../stringr/index.html">stringr</a>, <a href="../tableschema.r/index.html">tableschema.r</a>, tools, <a href="../urltools/index.html">urltools</a>, utils, <a href="../V8/index.html">V8</a></td>
</tr>
<tr>
<td>Suggests:</td>
<td><a href="../covr/index.html">covr</a>, <a href="../curl/index.html">curl</a>, <a href="../data.table/index.html">data.table</a>, <a href="../DBI/index.html">DBI</a>, <a href="../devtools/index.html">devtools</a>, <a href="../foreach/index.html">foreach</a>, <a href="../httptest/index.html">httptest</a>, <a href="../knitr/index.html">knitr</a>, <a href="../rmarkdown/index.html">rmarkdown</a>, <a href="../RSQLite/index.html">RSQLite</a>, <a href="../testthat/index.html">testthat</a></td>
</tr>
<tr>
<td>Published:</td>
<td>2020-05-06</td>
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
<td><a href="https://github.com/frictionlessdata/datapackage-r/issues">https://github.com/frictionlessdata/datapackage-r/issues</a></td>
</tr>
<tr>
<td>License:</td>
<td><a href="../../licenses/MIT">MIT</a> + file <a href="LICENSE">LICENSE</a></td>
</tr>
<tr>
<td>URL:</td>
<td><a href="https://github.com/frictionlessdata/datapackage-r">https://github.com/frictionlessdata/datapackage-r</a></td>
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
<td><a href="../../checks/check_results_datapackage.r.html">datapackage.r results</a></td>
</tr>
</table>
<h4>Downloads:</h4>
<table summary="Package datapackage.r downloads">
<tr>
<td> Reference&nbsp;manual: </td>
<td> <a href="datapackage.r.pdf"> datapackage.r.pdf </a> </td>
</tr>
<tr>
<td>Vignettes:</td>
<td>
<a href="vignettes/creating_data_packages_in_r.html">Creating Data Packages in R</a><br/>
<a href="vignettes/using_data_packages_in_r.Rmd.html">Using Data Packages in R</a><br/>
</td>
</tr>
<tr>
<td> Package&nbsp;source: </td>
<td> <a href="../../../src/contrib/datapackage.r_1.3.1.tar.gz"> datapackage.r_1.3.1.tar.gz </a> </td>
</tr>
<tr>
<td> Windows&nbsp;binaries: </td>
<td> r-devel: <a href="../../../bin/windows/contrib/4.1/datapackage.r_1.3.1.zip">datapackage.r_1.3.1.zip</a>, r-release: <a href="../../../bin/windows/contrib/4.0/datapackage.r_1.3.1.zip">datapackage.r_1.3.1.zip</a>, r-oldrel: <a href="../../../bin/windows/contrib/3.6/datapackage.r_1.3.1.zip">datapackage.r_1.3.1.zip</a> </td>
</tr>
<tr>
<td> macOS&nbsp;binaries: </td>
<td> r-release: <a href="../../../bin/macosx/contrib/4.0/datapackage.r_1.3.1.tgz">datapackage.r_1.3.1.tgz</a>, r-oldrel: <a href="../../../bin/macosx/el-capitan/contrib/3.6/datapackage.r_1.3.1.tgz">datapackage.r_1.3.1.tgz</a> </td>
</tr>
<tr>
<td> Old&nbsp;sources: </td>
<td> <a href="https://CRAN.R-project.org/src/contrib/Archive/datapackage.r"> datapackage.r archive </a> </td>
</tr>
</table>
<h4>Linking:</h4>
<p>Please use the canonical form
<a href="https://CRAN.R-project.org/package=datapackage.r"><samp>https://CRAN.R-project.org/package=datapackage.r</samp></a>
to link to this page.</p>
</body>
</html>
