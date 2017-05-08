
function getShow (show) {
    $.get(show, function (data) {
	$('body').html(data); })
}
