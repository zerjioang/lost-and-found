<?php   
	$bd_host = "localhost";   
	$bd_usuario = "root";   
	$bd_password = "d3m0";   
	$bd_base = "example";   
	$con = mysql_connect($bd_host, $bd_usuario, $bd_password);   
	mysql_select_db($bd_base, $con);   
?> 