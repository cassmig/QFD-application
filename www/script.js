 $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'file') {
          var fileInput = $('#file');
          var loadButton = $('#load_button');
          if (fileInput.val()) {
            loadButton.addClass('blink').removeClass('no-blink');
          } else {
            loadButton.removeClass('blink').addClass('no-blink');
          }
        }
      });
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'load_button') {
          var loadButton = $('#load_button');
          loadButton.addClass('no-blink');
        }
      });
      
      // 📌 Handles tab navigation by adding 'active' class to the clicked section
$(document).ready(function() {
    $(".nav-section").on("click", function() {
        $(".nav-section").removeClass("active"); // Remove active class from all
        $(this).addClass("active"); // Add active class to clicked section
    });
});

// 📌 Listen for messages from Shiny to update the active navigation section
Shiny.addCustomMessageHandler("updateNavigation", function(navId) {
    $(".nav-section").removeClass("active");
    $("#" + navId).addClass("active");
});



