<?php
function doc_type (){
    echo "<!doctype html>";
}
function draw_head ($title_string){
    ?>
    <head>
    <meta http-equiv="content-type" content="text/html; charset=windows-1251"/>
    <title>
    <?=$title_string?>
    </title>
    <link rel="stylesheet" type="text/css" href="style.css"/>
    </head>
    <?php
}
function get_arr_bazza_geo_tools (){
    $arr_strings = array();
    $files_patch = get_path_geo_tools_files();
$arr_strings = file($files_patch.'bazza.txt');
$arr_strings = array_reverse ($arr_strings);
foreach ($arr_strings as $string) {
    $arr_bazza[] = explode( '|', $string);
}
return $arr_bazza;
}
function draw_ver_geo_tools (){
    $bazza = get_arr_bazza_geo_tools();
    foreach ($bazza as $ver) {
        $name_file = $ver[0];
        $size_file = $ver[1];
        $create_file = $ver[2];
        $post_file = $ver[3];
        $note = $ver[4];
        ?>
<div class="geo_tools_file">
    <div class="date">
        <a href="files/<?=$name_file?>">
        <?=date('Y.m.d H:i', $create_file);?> 
        (<?=round ($size_file/1024/1024, 1);?> Мб) - загружено 
        <?=date('Y.m.d H:i', $post_file);?></a>
    </div>
    <div class="note">
    <?=$note?>
    </div>
</div>
        <?php
}
}
function get_path_geo_tools_files (){
    return 'files/';
}
function get_data_time_be_file_name ($file_name){
    $arr_name = explode( ' ', $file_name);
    $arr_data = explode( '.', $arr_name[1]);
    $arr_time = explode( '-', $arr_name[2]);
   // print_r ($arr_data);
   // print_r ($arr_time);
    return mktime($arr_time[0], $arr_time[1], 0, $arr_data[1], $arr_data[2], $arr_data[0]);
}
function add_record_to_bazza_geo_tools_file ($arrey){
    $files_patch = '../files/';
    $bazza_patch = '../files/bazza.txt';
    $file_name = $arrey['file_name'];
    $file_geo_tools_patch = $files_patch.$file_name;
    $file_size = filesize($file_geo_tools_patch);
    $file_create_time = get_data_time_be_file_name($file_name);
   // $file_create_time = filectime($file_geo_tools_patch);
    
    $file_post_time = time();
    $file_note = $arrey['file_note'];
    $record = "\r\n".$file_name.'|'.$file_size.'|'.$file_create_time.'|'.$file_post_time.'|'.$file_note;
    header("Content-type: text/plain charset=windows-1251");
    file_put_contents($bazza_patch,$record, FILE_APPEND);
}
?>
<?php
function draw_footer (){
    ?>
    <div class="footer"><img src="images/mail.png" width="160" height="16" alt="пишите письма"/></div>
    <?php
}
function draw_menu (){
    ?>
<div class="menu">
 <p><a href="index.php" >Зачем нужен этот сайт</a></p>
 <p><a href="what_it.php" >Geo_Tools что это?</a></p>
 <p><a href="realization.php" >Реализация Geo_Tools</a></p>
 <p><a href="ver.php" >Версия Geo_Tools</a></p>
 
 <p><a href="instructions.php" >Инструкции</a></p>
  <p><a href="download_geo_tools.php" >Скачать Geo_tools</a></p>
 <p><a href="university.php" >Мои университеты</a></p>
</div>
<?php
}
function draw_header (){
    ?>
    <div class="header"><h2>Geo_Tools</h2></div>
    <?php
}
?>
