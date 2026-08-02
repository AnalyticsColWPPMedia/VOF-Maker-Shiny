/* =======================================================*/
/* 1. Download Button Interaction                         */
/* =======================================================*/

$(document).ready(function() {
  // Usamos delegación de eventos en 'body' para que funcione 
  // incluso con botones creados dinámicamente en los módulos
  $('body').on('click', '.custom-download-btn', function() {
    var $btn = $(this);
    var buttonId = $btn.attr('id');
    
    $btn.addClass('btn-downloading');

    // Remove the class after a timeout
    setTimeout(function() {
      $btn.removeClass('btn-downloading');
    }, 3000);
  });
});

/* =======================================================*/
/* 2. Shiny Custom Message Handlers                       */
/* =======================================================*/

Shiny.addCustomMessageHandler('toggleDownloadButtonClass', function(message) {
  // NOTA: message.id debe incluir el namespace completo si viene de un módulo
  var button = $('#' + message.id);

  if (message.downloading) {
    button.addClass('btn-downloading');
  } else {
    button.removeClass('btn-downloading');
  }
});

/* =======================================================*/
/* 3. Datepicker Z-Index & Positioning Fix (MODULARIZADO) */
/* =======================================================*/

(function() {
  
  function moveDatepickerToBody($input) {
    var $dp = $('.datepicker.datepicker-dropdown');
    if (!$dp.length) return;

    if (!$dp.parent().is('body')) {
      $dp.detach().appendTo('body');
    }

    $dp.css({
      'z-index': 999999,
      'position': 'absolute'
    });

    var off = $input.offset();
    if (!off) return;

    var h = $input.outerHeight() || 0;
    
    $dp.css({
      top: (off.top + h + 6) + 'px',
      left: off.left + 'px'
    });
  }

  // CAMBIO: En lugar de buscar por nombre, buscamos por la clase '.vof-datepicker'
  // que añadiremos a los inputs en R.
  $(document).on('focus click', '.vof-datepicker input', function() {
    var $inp = $(this);
    setTimeout(function() {
      moveDatepickerToBody($inp);
    }, 0);
  });

  $(window).on('scroll resize', function() {
    var $active = $('.vof-datepicker input:focus');
    if ($active.length) {
      moveDatepickerToBody($active.first());
    }
  });
})();

// Handler para guardar en LocalStorage
Shiny.addCustomMessageHandler('saveToBrowser', function(data) {
    localStorage.setItem('vof_maker_history', data);
});

// Handler para borrar LocalStorage
Shiny.addCustomMessageHandler('clearBrowserStorage', function(msg) {
    localStorage.removeItem('vof_maker_history');
});

// Al conectarse, buscar si hay datos guardados y enviarlos a Shiny
$(document).on('shiny:connected', function() {
    var storedData = localStorage.getItem('vof_maker_history');
    if (storedData) {
        Shiny.setInputValue('restore_trigger', storedData);
    }
});