<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>CRAN - Package EDNE.EQ</title>
<link rel="stylesheet" type="text/css" href="../../CRAN_web.css" />
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="DC.identifier" content="https://CRAN.R-project.org/package=EDNE.EQ" />
<meta name="DC.publisher" content="Comprehensive R Archive Network (CRAN)" />
<meta name="og:title" content="EDNE.EQ: Implements the EDNE-Test for Equivalence" />
<meta name="og:description" content="Package implements the EDNE-test for equivalence according to Hoffelder et al. (2015) &amp;lt;&lt;a href=&quot;https://doi.org/10.1080%2F10543406.2014.920344&quot;&gt;doi:10.1080/10543406.2014.920344&lt;/a&gt;&amp;gt;. &quot;EDNE&quot; abbreviates &quot;Euclidean Distance between the Non-standardized Expected values&quot;. The EDNE-test for equivalence is a multivariate two-sample equivalence test. Distance measure of the test is the Euclidean distance. The test is an asymptotically valid test for the family of distributions fulfilling the assumptions of the multivariate central limit theorem (see Hoffelder et al.,2015). The function EDNE.EQ() implements the EDNE-test for equivalence according to Hoffelder et al. (2015). The function EDNE.EQ.dissolution.profiles() implements a variant of the EDNE-test for equivalence analyses of dissolution profiles (see Suarez-Sharp et al.,2020 &amp;lt;&lt;a href=&quot;https://doi.org/10.1208%2Fs12248-020-00458-9&quot;&gt;doi:10.1208/s12248-020-00458-9&lt;/a&gt;&amp;gt;). EDNE.EQ.dissolution.profiles() checks whether the quadratic mean of the differences of the expected values of both dissolution profile populations is statistically significantly smaller than 10 [\% of label claim]. The current regulatory standard approach for equivalence analyses of dissolution profiles is the similarity factor f2. The statistical hypotheses underlying EDNE.EQ.dissolution.profiles() coincide with the hypotheses for f2 (see Hoffelder et al.,2015, Suarez-Sharp et al., 2020)." />
<meta name="og:image" content="https://CRAN.R-project.org/CRANlogo.png" />
<meta name="og:type" content="website" />
<meta name="og:url" content="https://CRAN.R-project.org/package=EDNE.EQ" />
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@_R_Foundation" />
<style type="text/css">
  table td { vertical-align: top; }
</style>
</head>
<body>
<h2>EDNE.EQ: Implements the EDNE-Test for Equivalence</h2>
<p>Package implements the EDNE-test for equivalence
    according to Hoffelder et al. (2015) &lt;<a href="https://doi.org/10.1080%2F10543406.2014.920344">doi:10.1080/10543406.2014.920344</a>&gt;.
    "EDNE" abbreviates "Euclidean Distance between the 
    Non-standardized Expected values".
    The EDNE-test for equivalence is a multivariate two-sample equivalence test. 
    Distance measure of the test is the Euclidean distance.
    The test is an asymptotically valid test for the family of distributions 
    fulfilling the assumptions of the multivariate central limit theorem 
    (see Hoffelder et al.,2015).
    The function EDNE.EQ() implements the EDNE-test for equivalence 
    according to Hoffelder et al. (2015).
    The function EDNE.EQ.dissolution.profiles() implements a variant 
    of the EDNE-test for equivalence analyses of dissolution profiles
    (see Suarez-Sharp et al.,2020 &lt;<a href="https://doi.org/10.1208%2Fs12248-020-00458-9">doi:10.1208/s12248-020-00458-9</a>&gt;).
    EDNE.EQ.dissolution.profiles() checks whether the quadratic mean of the 
    differences of the expected values of both dissolution profile populations 
    is statistically significantly smaller than 10 [\% of label claim]. 
    The current regulatory standard approach for equivalence analyses of 
    dissolution profiles is the similarity factor f2.
    The statistical hypotheses underlying EDNE.EQ.dissolution.profiles()
    coincide with the hypotheses for f2 (see Hoffelder et al.,2015, 
    Suarez-Sharp et al., 2020).</p>
<table summary="Package EDNE.EQ summary">
<tr>
<td>Version:</td>
<td>1.0</td>
</tr>
<tr>
<td>Depends:</td>
<td>R (&ge; 3.5.0)</td>
</tr>
<tr>
<td>Imports:</td>
<td><a href="../MASS/index.html">MASS</a></td>
</tr>
<tr>
<td>Published:</td>
<td>2020-09-28</td>
</tr>
<tr>
<td>Author:</td>
<td>Thomas Hoffelder</td>
</tr>
<tr>
<td>Maintainer:</td>
<td>Thomas Hoffelder  &#x3c;&#x74;&#x68;&#x6f;&#x6d;&#x61;&#x73;&#x2e;&#x68;&#x6f;&#x66;&#x66;&#x65;&#x6c;&#x64;&#x65;&#x72;&#x20;&#x61;&#x74;&#x20;&#x62;&#x6f;&#x65;&#x68;&#x72;&#x69;&#x6e;&#x67;&#x65;&#x72;&#x2d;&#x69;&#x6e;&#x67;&#x65;&#x6c;&#x68;&#x65;&#x69;&#x6d;&#x2e;&#x63;&#x6f;&#x6d;&#x3e;</td>
</tr>
<tr>
<td>License:</td>
<td><a href="../../licenses/GPL-3">GPL-3</a></td>
</tr>
<tr>
<td>NeedsCompilation:</td>
<td>no</td>
</tr>
<tr>
<td>CRAN&nbsp;checks:</td>
<td><a href="../../checks/check_results_EDNE.EQ.html">EDNE.EQ results</a></td>
</tr>
</table>
<h4>Downloads:</h4>
<table summary="Package EDNE.EQ downloads">
<tr>
<td> Reference&nbsp;manual: </td>
<td> <a href="EDNE.EQ.pdf"> EDNE.EQ.pdf </a> </td>
</tr>
<tr>
<td> Package&nbsp;source: </td>
<td> <a href="../../../src/contrib/EDNE.EQ_1.0.tar.gz"> EDNE.EQ_1.0.tar.gz </a> </td>
</tr>
<tr>
<td> Windows&nbsp;binaries: </td>
<td> r-devel: <a href="../../../bin/windows/contrib/4.1/EDNE.EQ_1.0.zip">EDNE.EQ_1.0.zip</a>, r-release: <a href="../../../bin/windows/contrib/4.0/EDNE.EQ_1.0.zip">EDNE.EQ_1.0.zip</a>, r-oldrel: <a href="../../../bin/windows/contrib/3.6/EDNE.EQ_1.0.zip">EDNE.EQ_1.0.zip</a> </td>
</tr>
<tr>
<td> macOS&nbsp;binaries: </td>
<td> r-release: <a href="../../../bin/macosx/contrib/4.0/EDNE.EQ_1.0.tgz">EDNE.EQ_1.0.tgz</a>, r-oldrel: <a href="../../../bin/macosx/el-capitan/contrib/3.6/EDNE.EQ_1.0.tgz">EDNE.EQ_1.0.tgz</a> </td>
</tr>
</table>
<h4>Linking:</h4>
<p>Please use the canonical form
<a href="https://CRAN.R-project.org/package=EDNE.EQ"><samp>https://CRAN.R-project.org/package=EDNE.EQ</samp></a>
to link to this page.</p>
</body>
</html>
