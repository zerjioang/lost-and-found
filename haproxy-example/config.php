<?php   
	$bd_host = "localhost";   
	$bd_usuario = "root";   
	$bd_password = "d3m0";   
	$bd_base = "visitas";   
	$con = @mysqli_connect($bd_host, $bd_usuario, $bd_password, $bd_base);

	if (!$con) {
		echo "Error: " . mysqli_connect_error();
		exit();
	}
	echo 'Connected to MySQL';
?> 
