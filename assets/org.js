toggleBtn = document.getElementById('toggle-theme')

toggleBtn.addEventListener('click', function() {
  if (document.body.dataset.theme == 'dark') {
    document.body.dataset.theme = 'light'
    toggleBtn.innerHTML = 'dark theme'
  } else {
    document.body.dataset.theme = 'dark'
    toggleBtn.innerHTML = 'light theme'
  }

  document.body.classList.add('theme-transition')
  setTimeout(() => document.body.classList.remove('theme-transition'), 500)
})
