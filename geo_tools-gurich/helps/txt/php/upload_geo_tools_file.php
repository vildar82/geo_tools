<?php
include "lib.int.php";
$files_patch = '../files/';

 if(is_uploaded_file($_FILES['gt_file']['tmp_name'])) {
    $tmp_name = $_FILES['gt_file']['tmp_name'];
    $name = $_FILES['gt_file']['name'];
    move_uploaded_file($tmp_name, $files_patch.$name);
    $note = $_POST['note'];
    $arr_record ['file_name']= $name;
    $arr_record ['file_note']= $note;
    add_record_to_bazza_geo_tools_file($arr_record);
}
else{
    $error = $_FILES['gt_file']['error'];
    switch ($error){
        case UPLOAD_ERR_OK: echo '��� �����!';break;
        case UPLOAD_ERR_INI_SIZE: echo '���� ������� �������!';break;
        case UPLOAD_ERR_FORM_SIZE: echo '���� ������� ������� �� �����!';break;
        case UPLOAD_ERR_PARTIAL: echo '����������� ���� ��� ������� ������ ��������!';break;
        case UPLOAD_ERR_NO_FILE: echo '���� �� ��� ��������';break;
        default: echo '�������� PHP!';
    }
}
header('Location: ' . "../download_geo_tools.php");
?>