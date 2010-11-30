
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->
<!-- thpe: begin of user supplied material -->

<link rel="stylesheet" href="style.css">


<h2>What is <span style="color: rgb(153, 0, 0);">simecol</span> and
what is <span style="color: rgb(153, 0,
0);">simecolModels</span>?</h2>

<p><span style="font-family: monospace; font-weight: bold; color:
rgb(153, 0, 0);"><span style="font-family:
sans-serif;">simecol</span></span> (<span style="color: rgb(153, 0, 0);
font-weight: bold;">sim</span>ulation of <span style="color: rgb(153, 0,
0); font-weight: bold;">ecol</span>ogical systems) is an <span
style="font-weight: bold; color: rgb(51, 51, 153);">R</span> package
which is based on an object oriented paradigm for the
implementation of dynamic simulation models.</p>

<p> The <code style="color: rgb(153, 0, 0);">simecol</code> package is
intended to give users (students and scientists) an interactive
environment to implement, distribute, simulate and document basic and
advanced ecological models without the need to write long simulation
programs. For this purpose, an object oriented approach is developed,
which should provide a consistent but still flexible and extensible way
to implement simulation models of different types, namely </p>

<ul>
  <li>ordinary differential equation (ODE) models,</li>
  <li>non-spatial individual-based models,</li>
  <li>grid-oriented individual-based models,</li>
  <li>particle diffusion-type models</li>
  <li>and more.</li>
</ul>

<p> Each simulation model is implemented as <code style="color: rgb(153,
0, 0);">simecol</code><span style="color: rgb(153, 0, 0);">
</span>simulation model object with components <code style="color:
rgb(153, 0, 0);">main</code><span style="color: rgb(153, 0,
0);">,</span> holding the main model equations, rules or arbitrary
program code <span style="font-family: monospace; color: rgb(153, 0,
0);">equations</span> (optional, a list of possibly nested sub-models or
sub-equations ), <code style="color: rgb(153, 0, 0);">parms</code><span
style="color: rgb(153, 0, 0);"> </span>with model parameters,<span
style="font-family: monospace; color: rgb(153, 0, 0);"> init</span> with
the initial state, <span style="font-family: monospace; color: rgb(153,
0, 0);">inputs</span> (optional) for external input data and <code
style="color: rgb(153, 0, 0);">times</code><span style="color: rgb(153,
0, 0);"> </span>to define the simulation time and the time steps used.
</p>

<p><span style="font-weight: bold; color: rgb(153, 0, 0); font-family: monospace;">simecolModels</span>
is a simulation model collection, together with additional classes, demos and experimental code.


<h2>Downloads and Project Pages</h2>

<table style="text-align: left; width: 100%;" border="1" cellpadding="4" cellspacing="0">
<tr>

<td>
<h3>simecol</h3>

<ul>
<li><strong>Official Version (Download from CRAN)</strong></li>
<ul>
<li><a href="http://cran.r-project.org/web/packages/simecol/">Sources, Binaries, Docs on CRAN</a></li>
</ul>
<li><strong>Development Version (Download from R-Forge)</strong></li>
<ul>
<li><a href="http://r-forge.r-project.org/projects/simecol/">Project Overview Page</a></li>
<li><a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/?root=simecol">Inspect Source Code</a></li>
<li><a href="http://r-forge.r-project.org/R/?group_id=146">Download Packages</a></li>
</ul>
</ul>
</td>

<td>
<h3>simecolModels</h3>

<ul>
<li><strong>Official Version</strong></li>
<ul>
<li>not yet released</li>
</ul>
<li><strong>Development Version (Download from R-Forge)</strong></li>
<ul>
<li><a href="http://r-forge.r-project.org/projects/simecol/">Project Overview Page</a></li>
<li><a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/?root=simecol">Inspect Source Code</a></li>
<li><a href="http://r-forge.r-project.org/R/?group_id=146">Download Packages</a></li>
</ul>
</ul>
</td>

</tr>

</table>

<h2>Author</h2>

<ul>
<li> Thomas Petzoldt, Technische Universit&auml;t Dresden, Institut f&uuml;r Hydrobiologie </li>
<li> <a href="http://tu-dresden.de/Members/thomas.petzoldt">http://tu-dresden.de/Members/thomas.petzoldt </a> </li>
</ul> 


<h2>Prerequisites</h2>

<ul>

  <li><span style="font-weight: bold; color: rgb(153, 0, 0);
  font-family: monospace;">simecol</span> is based on <span
  style="font-weight: bold; color: rgb(51, 51, 153);">R</span>, a
  freely available system for statistical computation and
  graphics. Current versions are tested with <a style="font-weight: bold;"
  href="http://cran.r-project.org">R version 2.11.0</a> or above.</li>

  <li>simecol depends on the <span style="color: rgb(153, 0, 0);
  font-family: monospace; font-weight: bold;">deSolve</span> package
  for numerical integration.</li>

  <li>optional: the <span style="color: rgb(153, 0, 0); font-family:
  monospace; font-weight: bold;">tcltk</span> package is required for
  the graphical parameter editing functions </li>

  <li>Operating systems: All operating systems on which <span
  style="font-weight: bold; color: rgb(51, 51, 153);">R</span> can be
  installed: Linux, Windows, Mac, different UNIXes </li>

</ul>

<h2>Installation</h2>


<p>The packages can be installed directly from the internet
within <span style="font-weight: bold; color: rgb(51, 51,
153);">R</span> either via the menu (on Windows) or via the <span
style="font-weight: bold; color: rgb(51, 51, 153);">R</span> command
line:</p>

<h4>Stable Version:</h4>

<table border="1" cellpadding="4" cellspacing="0">
  <tbody>
    <tr>
      <td>
      <span style="font-family: monospace;">&gt;install.packages("simecol")</span><br>
      </td>
    </tr>
  </tbody>
</table>


<h4>Development Versions:</h4>
<table border="1" cellpadding="4" cellspacing="0">
  <tbody>
    <tr>
      <td>
      <span style="font-family: monospace;">&gt;install.packages("simecol", repos="http://R-Forge.R-project.org")</span><br>
      <span style="font-family: monospace;">&gt;install.packages("simecolModels", repos="http://R-Forge.R-project.org")</span><br>
      </td>
    </tr>
  </tbody>
</table>


<h4>Subversion (SVN) Access</h4>

<ul>
  <li> Anonymous SVN access (read only): <span style="font-family:
  monospace;">svn checkout
  svn://svn.r-forge.r-project.org/svnroot/simecol</span>

  <li>Write access to the simecol SVN repository can be allowed upon
  <a href="mailto:thomas.petzoldt@tu-dresden.de">request</a>.</li>
</ul>

<h2>Documentation</h2>

<h3>Reference Manual</h3>

<ul>
  <li><a
  href="http://cran.r-project.org/web/packages/simecol/simecol.pdf">Online
  documentation</a> of the package (functions, examples).</li>
</ul>

<h3>Articles</h3>

<ul>

  <li>Petzoldt, T. and K. Rinke (2007).  simecol: An Object-Oriented
  Framework for Ecological Modeling in R.  Journal of Statistical
  Software, 22(9), 1--31.  URL http://www.jstatsoft.org/v22/i09 <a
  href="http://www.jstatsoft.org/v22/i09">(pdf)</a>.</li>

  <li>Petzoldt, T. (2003). R as a Simulation Platform in Ecological
  Modelling. R-News 3(3), 8--16. <a
  href="http://cran.r-project.org/doc/Rnews/Rnews_2003-3.pdf">(pdf)</a></li>

</ul>

<h3>Additional Material</h3>

<ul>
  
  <li><a
  href="http://hhbio.wasser.tu-dresden.de/projects/simecol/useR-2008-lecture.pdf">Lecture
  slides</a> from the <b>useR-2008</b> conference about <span
  style="font-weight: bold; color: rgb(153, 0, 0); font-family:
  monospace;">simecol</span> (including the most recent features)

  <li><a
  href="http://hhbio.wasser.tu-dresden.de/projects/simecol/useR-2006-poster.pdf">Poster</a>
  and <a
  href="http://hhbio.wasser.tu-dresden.de/projects/simecol/useR2006-slides">lecture
  slides</a> of the <a
  href="http://www.r-project.org/useR-2006/">useR-2006</a> conference
  comparing OOP approaches and introducing <span style="font-weight:
  bold; color: rgb(153, 0, 0); font-family:
  monospace;">simecol</span><span style="font-weight: bold; color:
  rgb(153, 0, 0);"></span>.</li>

  <li><a
  href="http://hhbio.wasser.tu-dresden.de/projects/simecol/useR-2004.pdf">Lecture
  slides</a> from the useR-2004 conference about the <span
  style="font-weight: bold; color: rgb(153, 0, 0); font-family:
  monospace;">simecol</span> concept and the (old) S3 list-based
  approach.<br> </li>

  <li><a
  href="http://hhbio.wasser.tu-dresden.de/projects/modlim/doc/modlim.pdf">German
  tutorial</a> describing ecological modelling with <span
  style="font-weight: bold; color: rgb(51, 51, 153);">R</span> in
  general (a little bit outdated and still without <span
  style="font-weight: bold; color: rgb(153, 0, 0); font-family:
  monospace;">simecol</span>).</li>

</ul>

<h2>Examples<span style="color: rgb(192, 192, 192);"></span></h2>
<table style="text-align: left; width: 100%;" border="1" cellpadding="4" cellspacing="0">
  <tbody>
    <tr>
      <td style="width: 30%; vertical-align: top; font-weight: bold;">
      <p>A basic Lotka-Volterra model</p>
      </td>
      <td style="width: 79%;"><span style="font-family: monospace;">library("simecol")</span><br style="font-family: monospace;">
      <span style="font-family: monospace;">data(lv, package="simecol")</span><br style="font-family: monospace;">
      <span style="font-family: monospace;">plot(sim(lv))</span></td>
    </tr>
    <tr>
      <td style="vertical-align: top;">
      <p><span style="font-weight: bold;">The classical Conway's Game of Life</span></p>
      </td>
      <td><span style="font-family: monospace;"></span>
<span style="font-family: monospace;">library("simecol")<br>
data(conway</span><span style="font-family: monospace;">, package="simecol"</span><span style="font-family: monospace;">)<br>
plot(sim(conway))<br>
m &lt;- matrix(0, 40, 40)<br>
m[5:35,19:21] &lt;-1<br>
init(conway) &lt;- m<br>
sim(conway, animate=TRUE, delay=100, col=c("white", "green"), axes=FALSE)
</span><br style="font-family: monospace;">
    </td>
    </tr>
    <tr>
      <td style="vertical-align: top;">
      <p style="font-weight: bold;">Object oriented (S4 based) representation of a simulation model object (simecol object).</p>
      <p><br>
      </p>
      <p><br>
      </p>
      </td>
      <td><span style="font-family: monospace; color: rgb(204, 0, 0);"></span>
<span style="font-family: monospace;">
<span style="color: rgb(204, 0, 0);">
library("simecol") # load the package first!
</span><br><br>
</span>

<span style="font-family: monospace;">conway &lt;- new("gridModel",<br>
&nbsp;&nbsp;&nbsp; main = function(time, init, parms) {<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; x&nbsp;&nbsp; &lt;- init<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; srv &lt;- parms$srv<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; gen &lt;- parms$gen<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; n&nbsp;&nbsp; &lt;- nrow(x)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; m&nbsp;&nbsp; &lt;- ncol(x)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; nb&nbsp; &lt;- eightneighbours(x)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ## survival rule <br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; xsrv &lt;- ifelse(x &gt; 0 &amp; (nb %in% srv), 1, 0)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ## generation rule<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; xgen &lt;- ifelse(x == 0 &amp; (nb %in% gen), 1, 0)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; x&nbsp;&nbsp;&nbsp; &lt;- as.numeric((xgen + xsrv)&gt;0)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; dim(x) &lt;- c(n,m)<br>
&nbsp;&nbsp;&nbsp; x<br>
&nbsp;&nbsp;&nbsp; },<br>
&nbsp;&nbsp;&nbsp; parms = list(srv=c(2, 3), gen=3),<br>
&nbsp;&nbsp;&nbsp; times = c(from=1, to=10, by=1),<br>
&nbsp;&nbsp;&nbsp; init = matrix(round(runif(40*40)), nrow=40, ncol=40),<br>
&nbsp;&nbsp;&nbsp; solver = "iteration"<br>
)<br><br>
## and to run this example:<br><br>
plot(sim(conway))</span></td>
</tr>

</tbody>
</table>

<h2>License</h2>

<ul>

  <li><span style="color: rgb(153, 0, 0); font-family: monospace;
  font-weight: bold;">simecol</span>, like <span style="font-weight:
  bold; color: rgb(51, 51, 153);">R</span>, is free open source
  software licensed under the <a
  href="http://www.gnu.org/licenses/licenses.html" target="_blank">GNU
  Public License</a> (GPL 2.0 or above).</li>

  <li>According to this <a
  href="http://www.gnu.org/licenses/licenses.html"
  target="_blank">license</a> the software is provided as is and comes
  <span style="font-weight: bold;">WITHOUT WARRANTY.</span>.</li>

</ul>


<h2>Links</h2>

<ul>

<li><a href="http://cran.r-project.org/web/packages/simecol/">Stable Release Version from CRAN</a>,</li>
<li><a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">R-Forge Project Summary Page</a>,</li>
<li><a href="http://r-forge.r-project.org/R/?group_id=146">Nightly Builds</a>,</li>
<li>Documentation, books, papers for the <a href="http://desolve.r-forge.r-project.org/">deSolve</a>package, used for differential equation models.</li>
<li><a href="http://tu-dresden.de/Members/thomas.petzoldt">The Author's Homepage.</a></li>
</ul>
<!-- thpe: end of user supplied material -->



</body>
</html>
