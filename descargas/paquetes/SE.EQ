<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<title>CRAN - Package SE.EQ</title>
<link rel="stylesheet" type="text/css" href="../../CRAN_web.css" />
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="DC.identifier" content="https://CRAN.R-project.org/package=SE.EQ" />
<meta name="DC.publisher" content="Comprehensive R Archive Network (CRAN)" />
<meta name="og:title" content="SE.EQ: SE-Test for Equivalence" />
<meta name="og:description" content="Implements the SE-test for equivalence according to Hoffelder et al. (2015) &amp;lt;&lt;a href=&quot;https://doi.org/10.1080%2F10543406.2014.920344&quot;&gt;doi:10.1080/10543406.2014.920344&lt;/a&gt;&amp;gt;. The SE-test for equivalence is a multivariate two-sample equivalence test. Distance measure of the test is the sum of standardized differences between the expected values or in other words: the sum of effect sizes (SE) of all components of the two multivariate samples. The test is an asymptotically valid test for normally distributed data (see Hoffelder et al.,2015). The function SE.EQ() implements the SE-test for equivalence according to Hoffelder et al. (2015). The function SE.EQ.dissolution.profiles() implements a variant of the SE-test for equivalence for similarity analyses of dissolution profiles as mentioned in Suarez-Sharp et al.(2020) &amp;lt;&lt;a href=&quot;https://doi.org/10.1208%2Fs12248-020-00458-9&quot;&gt;doi:10.1208/s12248-020-00458-9&lt;/a&gt;&amp;gt;). The equivalence margin used in SE.EQ.dissolution.profiles() is analogically defined as for the T2EQ approach according to Hoffelder (2019) &amp;lt;&lt;a href=&quot;https://doi.org/10.1002%2Fbimj.201700257&quot;&gt;doi:10.1002/bimj.201700257&lt;/a&gt;&amp;gt;) by means of a systematic shift in location of 10 [\% of label claim] of both dissolution profile populations. SE.EQ.dissolution.profiles() checks whether the weighted mean of the differences of the expected values of both dissolution profile populations is statistically significantly smaller than 10 [\% of label claim]. The weights are built up by the inverse variances." />
<meta name="og:image" content="https://CRAN.R-project.org/CRANlogo.png" />
<meta name="og:type" content="website" />
<meta name="og:url" content="https://CRAN.R-project.org/package=SE.EQ" />
<meta name="twitter:card" content="summary" />
<meta name="twitter:site" content="@_R_Foundation" />
<style type="text/css">
  table td { vertical-align: top; }
</style>
</head>
<body>
<h2>SE.EQ: SE-Test for Equivalence</h2>
<p>Implements the SE-test for equivalence
    according to Hoffelder et al. (2015) &lt;<a href="https://doi.org/10.1080%2F10543406.2014.920344">doi:10.1080/10543406.2014.920344</a>&gt;.
    The SE-test for equivalence is a multivariate two-sample equivalence test. 
    Distance measure of the test is the sum of standardized differences
    between the expected values or in other words: the sum of effect sizes (SE)
    of all components of the two multivariate samples. 
    The test is an asymptotically valid test for normally distributed data 
    (see Hoffelder et al.,2015).
    The function SE.EQ() implements the SE-test for equivalence 
    according to Hoffelder et al. (2015).
    The function SE.EQ.dissolution.profiles() implements a variant 
    of the SE-test for equivalence for similarity analyses of dissolution 
    profiles as mentioned in Suarez-Sharp et al.(2020) 
    &lt;<a href="https://doi.org/10.1208%2Fs12248-020-00458-9">doi:10.1208/s12248-020-00458-9</a>&gt;). The equivalence margin used in 
    SE.EQ.dissolution.profiles() is analogically defined as for the T2EQ
    approach according to Hoffelder (2019) &lt;<a href="https://doi.org/10.1002%2Fbimj.201700257">doi:10.1002/bimj.201700257</a>&gt;) 
    by means of a systematic shift in location
    of 10 [\% of label claim] of both dissolution profile populations. 
    SE.EQ.dissolution.profiles() checks whether the weighted mean of the 
    differences of the expected values of both dissolution profile populations 
    is statistically significantly smaller than 10 [\% of label claim]. The 
    weights are built up by the inverse variances.</p>
<table summary="Package SE.EQ summary">
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
<td>2020-10-13</td>
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
<td><a href="../../checks/check_results_SE.EQ.html">SE.EQ results</a></td>
</tr>
</table>
<h4>Downloads:</h4>
<table summary="Package SE.EQ downloads">
<tr>
<td> Reference&nbsp;manual: </td>
<td> <a href="SE.EQ.pdf"> SE.EQ.pdf </a> </td>
</tr>
<tr>
<td> Package&nbsp;source: </td>
<td> <a href="../../../src/contrib/SE.EQ_1.0.tar.gz"> SE.EQ_1.0.tar.gz </a> </td>
</tr>
<tr>
<td> Windows&nbsp;binaries: </td>
<td> r-devel: <a href="../../../bin/windows/contrib/4.1/SE.EQ_1.0.zip">SE.EQ_1.0.zip</a>, r-release: <a href="../../../bin/windows/contrib/4.0/SE.EQ_1.0.zip">SE.EQ_1.0.zip</a>, r-oldrel: <a href="../../../bin/windows/contrib/3.6/SE.EQ_1.0.zip">SE.EQ_1.0.zip</a> </td>
</tr>
<tr>
<td> macOS&nbsp;binaries: </td>
<td> r-release: <a href="../../../bin/macosx/contrib/4.0/SE.EQ_1.0.tgz">SE.EQ_1.0.tgz</a>, r-oldrel: <a href="../../../bin/macosx/el-capitan/contrib/3.6/SE.EQ_1.0.tgz">SE.EQ_1.0.tgz</a> </td>
</tr>
</table>
<h4>Linking:</h4>
<p>Please use the canonical form
<a href="https://CRAN.R-project.org/package=SE.EQ"><samp>https://CRAN.R-project.org/package=SE.EQ</samp></a>
to link to this page.</p>
</body>
</html>
