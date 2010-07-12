
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

<!-- R-Forge Logo -->
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

<p> 
How to use the debugger<br>
=======================<br>
1. source the debugger<br>
   source("idbg.R")<br>
2. Set a breakpoint in the function to debug<br>
   idbg.bp("foo")<br>
3. Call the function<br>
   foo(1,2,3)<br>
4. The debugger will stop at the first line of foo<br>
5. Use the following commands<br>
<br>
h - help. Print this message<br>
n - next. Empty line is the same as 'n'<br>
s - step. Step  into a function<br>
o - out. Step out of a function<br>
c - continue. Continue running<br>
q - quit. Exit the debugger<br>
b - print breakpoints <br>
b <func_name> [FALSE] - set/unset a breakpoint in the first line of function<br>
b <line_number> [FALSE] - set/unset a breakpoint in current function<br>
b <func_name> <line_number> [FALSE] - set/unset a breakpoint in function at line_number<br>
w - where. Print the stack<br>
u - up. Go up the stack<br>
d - down. Go down the stack<br>
l [nlines] - list. Print nlines of source before and after current position<br>
l [nlines_back] [n_lines_forward] - list. Print source around current position<br>
x expr - execute.  Evaluate expr. Any expression that doesn't match the above options will also be executed<br>
<br>
<br>
Enjoy,<br>
Ronen<br>

 </p>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
