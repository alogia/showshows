
function getShow (show) {
    $.get(show, function (data) {
	$('body').html(data); })
};

function update () {
    $('.accordion').accordion();
};

$(document).ready( function () {
    update();
});

