$(document).ready(function() {
        
        // Making right sidebar draggable by its header
        var isDragging = false;
        var currentSidebar = null;
        var startX, startY, startLeft, startTop;
        
        $(document).on('mousedown', '.right-sidebar .sidebar-header', function(e) {
          if ($(e.target).closest('.close-sidebar').length > 0) return;
          
          currentSidebar = $(this).closest('.right-sidebar');
          isDragging = true;
          currentSidebar.addClass('dragging');
          
          startX = e.clientX;
          startY = e.clientY;
          
          var offset = currentSidebar.offset();
          startLeft = offset.left;
          startTop = offset.top;
          
          e.preventDefault();
          e.stopPropagation();
        });
        
        $(document).on('mousemove', function(e) {
          if (!isDragging || !currentSidebar) return;
          
          var deltaX = e.clientX - startX;
          var deltaY = e.clientY - startY;
          
          var newLeft = startLeft + deltaX;
          var newTop = startTop + deltaY;
          
          var maxLeft = $(window).width() - currentSidebar.outerWidth();
          var maxTop = $(window).height() - currentSidebar.outerHeight();
          
          newLeft = Math.max(0, Math.min(newLeft, maxLeft));
          newTop = Math.max(56, Math.min(newTop, maxTop));
          
          currentSidebar.css({
            left: newLeft + 'px',
            top: newTop + 'px',
            right: 'auto'
          });
          
          e.preventDefault();
        });
        
        $(document).on('mouseup', function(e) {
          if (isDragging) {
            isDragging = false;
            if (currentSidebar) {
              currentSidebar.removeClass('dragging');
              currentSidebar = null;
            }
          }
        });
        
        // Toggling left sidebar
        $('#toggleLeft').click(function() {
          $('.left-sidebar').toggleClass('collapsed');
          if ($('.left-sidebar').hasClass('collapsed')) {
            $(this).html('Show Controls ▶'); 
          } else {
            $(this).html('Hide Controls ◀'); 
          }
        });
        
        // Toggling search container
        $('#toggleSearch').click(function() {
          $('.search-container').toggleClass('collapsed');
          if ($('.search-container').hasClass('collapsed')) {
            $(this).html('Show Search ◀'); 
          } else {
            $(this).html('Hide Search ▶'); 
          }
        });
        
        // Closing sidebars when clicking map
        $('#map').on('click', function(e) {
          if ($(e.target).closest('.leaflet-control-container, .leaflet-popup, .left-sidebar, .search-container, .sidebar-toggle, .search-toggle, .right-sidebar').length === 0) {
            if (!$('.left-sidebar').hasClass('collapsed')) {
              $('.left-sidebar').addClass('collapsed');
              $('#toggleLeft').html('Show Controls ▶');
            }
            if (!$('.search-container').hasClass('collapsed')) {
              $('.search-container').addClass('collapsed');
              $('#toggleSearch').html('Show Search ◀');
            }
            $('.right-sidebar').removeClass('active');
            
            if ($('#bottom-drawer').hasClass('open')) {
              $('#bottom-drawer').removeClass('open');
              $('#table-backdrop').removeClass('active');
              $('#toggle-drawer').text('Show Data Table ▲');
              $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
            }
          }
        });
        
        // Closing table when clicking backdrop
        $('#table-backdrop').on('click', function() {
          $('#bottom-drawer').removeClass('open');
          $(this).removeClass('active');
          $('#toggle-drawer').text('Show Data Table ▲');
          $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
        });
        
        // Closing right sidebar
        $(document).on('click', '.close-sidebar', function() {
          $('.right-sidebar').removeClass('active');
        });
        
        // Toggling data table drawer
        function toggleDataTable() {
          $('#bottom-drawer').toggleClass('open');
          $('#table-backdrop').toggleClass('active');
          var isOpen = $('#bottom-drawer').hasClass('open');
          
          var handleText = isOpen ? 'Hide Data Table ▼' : 'Show Data Table ▲';
          $('#toggle-drawer').text(handleText);
          
          var btnHtml = isOpen ? '<i class=\"fas fa-table\"></i> Hide Data Table' : '<i class=\"fas fa-table\"></i> Show Data Table';
          $('#btn-show-table').html(btnHtml);
          
          if(isOpen) {
            setTimeout(function() { $(window).trigger('resize'); }, 300);
          }
        }
        
        // Showing about modal
        $('#about_btn').click(function() {
          Shiny.setInputValue('show_about', Math.random());
        });

        $('#toggle-drawer').click(toggleDataTable);
        $('#btn-show-table').click(toggleDataTable);
        
        // Showing loading overlay when updating map
        $('#update_map').on('click', function() {
          $('#loading-overlay').addClass('active');
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
        
        // Collapsing sidebars when searching address
        $('#search_address').on('click', function() {
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
        
        // Collapsing sidebars when searching district
        $('#search_district').on('click', function() {
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
      });
      
      // Hiding loading overlay
      Shiny.addCustomMessageHandler('hide_loading', function(message) {
        $('#loading-overlay').removeClass('active');
      });