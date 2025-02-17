document.addEventListener('DOMContentLoaded', function() {
  // When a tab is clicked, update the Shiny input
  document.querySelectorAll('.nav-item').forEach(function(tab) {
    tab.addEventListener('click', function() {
      var tabName = this.querySelector('span.nav-link').textContent.trim();
      Shiny.setInputValue('active_tab', tabName);
    });
  });
});