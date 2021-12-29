/*
Modal image viewer
Adapted by Dan Chaltiel from https://www.jqueryscript.net/lightbox/image-modal-zoom-in-out.html
*/
(function ($) {
    $(function () {
        //console.log('image-popup loaded')
        $('.section img').addClass('zz_image')
        
        var body = $('body');
        var container = $('<div id="zz_frame"></div>');
        var helper = $('<span id="zz_helper"></span>');
        var close_btn = $('<button class="zz_close">&times;</button>');
        var button_zoom = $('<div class="zz_controls"><button type="button" class="zz_zoom-in">Zoom In</button><button type="button" class="zz_zoom-out">Zoom Out</button></div>');
        var image = $('<img src="" />');
    
        $('.zz_image').click( function(e) {
            e.preventDefault();
            uri = $(this).attr('src');
            image.attr('src', uri);
    
            container.append(helper);
            container.append(image);
            container.append(close_btn);
            body.append(container);
        });
    
        $('body').on('click', '.zz_close, #zz_frame, #zz_frame img', function(e) {
            $('#zz_frame').remove();
        })

    });
})(jQuery);      
