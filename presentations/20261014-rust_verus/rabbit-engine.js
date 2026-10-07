'use strict';

// Audience timer inspired by Rabbit; no Rabbit image assets are used.
const TIMER_STYLE = `:root { --race-height: 96px; }
#rabbit-race { position: fixed; inset: auto 0 0; height: var(--race-height); box-sizing: border-box; z-index: 10000; padding: 4px 28px; background: transparent; color: #243329; font: 15px/1.2 system-ui, sans-serif; pointer-events: none; cursor: auto; }
#rabbit-race .race-head { display: flex; justify-content: flex-end; align-items: center; gap: 12px; height: 25px; }
#rabbit-race .race-controls { background: #ffffffd9; border-radius: 6px; padding: 2px 6px; }
#rabbit-race .race-controls { display: flex; align-items: center; gap: 8px; opacity: 0; transition: opacity .2s; pointer-events: auto; }
#rabbit-race .race-controls:hover, #rabbit-race .race-controls:focus-within, #rabbit-race[data-stopped] .race-controls { opacity: 1; }
#rabbit-race button { font: inherit; border: 1px solid #b4c5b6; border-radius: 6px; padding: 3px 9px; background: white; color: #243329; cursor: pointer; }
#race-lanes { position: relative; margin: 0 26px; height: 54px; }
.race-animal { position: absolute; left: 0; transform: translateX(-50%) scaleX(-1); font-size: 28px; line-height: 28px; transition: left .15s linear; }
#race-rabbit { top: 0; } #race-turtle { top: 28px; }
body[data-bespoke-view="overview"] #rabbit-race,
body[data-bespoke-view="presenter"] #rabbit-race,
body[data-bespoke-view="next"] #rabbit-race { display: none; }
@media print { #rabbit-race { display: none; } }
@media (max-width: 600px) { #rabbit-race { padding-inline: 12px; font-size: 12px; } #race-help { display: none; } }
@media (prefers-reduced-motion: reduce) { .race-animal { transition: none; } }`;

const TIMER_MARKUP = `<aside id="rabbit-race" aria-label="Presentation pace: slides and elapsed time">
  <div class="race-head">
    <div class="race-controls">
      <span id="race-help">T: start/pause &middot; Shift+R: reset</span>
      <button id="race-toggle" type="button">Start</button>
      <button id="race-reset" type="button">Reset</button>
    </div>
  </div>
  <div id="race-lanes">
    <span id="race-rabbit" class="race-animal" title="Slide progress" aria-label="Rabbit: slide progress">&#128007;</span>
    <span id="race-turtle" class="race-animal" title="Elapsed time" aria-label="Turtle: elapsed time">&#128034;</span>
  </div>
</aside>`;

// This function is serialized into the HTML. Keep all browser logic inside
// it, and pass configuration as arguments rather than capturing Node values.
function initializeTimer(duration, css, markup) {
  if (document.getElementById('rabbit-race') ||
      !document.querySelector('svg.bespoke-marp-slide')) return;
  const style = document.createElement('style');
  style.textContent = css;
  document.head.appendChild(style);
  document.body.insertAdjacentHTML('beforeend', markup);
  const key = 'marp-rabbit-timer:' + location.pathname + ':' + duration;
  const bar = document.getElementById('rabbit-race');
  const toggle = document.getElementById('race-toggle');
  const rabbit = document.getElementById('race-rabbit');
  const turtle = document.getElementById('race-turtle');
  // Marp Bespoke exposes active slides through CSS classes. Observe only
  // these SVGs, rather than mutations caused by our own timer updates.
  const slides = Array.from(document.querySelectorAll('svg.bespoke-marp-slide'));
  let state = { elapsed: 0, startedAt: null };
  const valid = s => s && Number.isFinite(s.elapsed) && s.elapsed >= 0 &&
    (s.startedAt === null || Number.isFinite(s.startedAt));
  try {
    const saved = JSON.parse(localStorage.getItem(key));
    if (valid(saved)) state = saved;
  } catch (_) { /* Storage can be unavailable for local files. */ }
  const elapsed = () => state.elapsed + (state.startedAt === null ? 0 : Math.max(0, Date.now() - state.startedAt));
  const positionOverlay = () => {
    const slide = slides.find(s => s.classList.contains('bespoke-marp-active'));
    if (!slide || document.body.dataset.bespokeView) return;
    const rect = slide.getBoundingClientRect();
    const view = slide.viewBox.baseVal;
    if (!rect.width || !rect.height || !view.width || !view.height) return;
    // Marp fits a fixed-aspect SVG inside the viewport. Follow the visible
    // slide, including letterboxing, without changing the slide's dimensions.
    const scale = Math.min(rect.width / view.width, rect.height / view.height);
    const width = view.width * scale;
    const height = view.height * scale;
    bar.style.left = (rect.left + (rect.width - width) / 2) + 'px';
    bar.style.top = (rect.top + (rect.height - height) / 2 + height - bar.offsetHeight) + 'px';
    bar.style.width = width + 'px';
    bar.style.bottom = 'auto';
  };
  const render = () => {
    const current = Math.max(0, slides.findIndex(s => s.classList.contains('bespoke-marp-active')));
    const time = elapsed();
    const total = Math.max(1, slides.length);
    // The first slide is the start; the last slide is the finish.
    rabbit.style.left = (total === 1 ? 100 : current / (total - 1) * 100) + '%';
    turtle.style.left = Math.min(100, time / duration * 100) + '%';
    toggle.textContent = state.startedAt === null ? (state.elapsed ? 'Resume' : 'Start') : 'Pause';
    bar.toggleAttribute('data-stopped', state.startedAt === null);
  };
  const save = () => {
    try { localStorage.setItem(key, JSON.stringify(state)); } catch (_) {}
    render();
  };
  const startPause = () => {
    if (state.startedAt === null) state.startedAt = Date.now();
    else state = { elapsed: elapsed(), startedAt: null };
    save();
  };
  const reset = () => { state = { elapsed: 0, startedAt: null }; save(); };
  toggle.addEventListener('click', startPause);
  document.getElementById('race-reset').addEventListener('click', reset);
  // Prevent Marp's click navigation from handling timer controls.
  for (const event of ['click', 'pointerdown', 'touchstart']) {
    bar.addEventListener(event, e => e.stopPropagation());
  }
  document.addEventListener('keydown', e => {
    if (e.repeat || e.ctrlKey || e.metaKey || e.altKey ||
        e.target.closest('input,textarea,[contenteditable="true"]')) return;
    if (e.key.toLowerCase() === 't' || (e.shiftKey && e.key.toLowerCase() === 'r')) {
      e.preventDefault(); e.stopImmediatePropagation();
      e.key.toLowerCase() === 't' ? startPause() : reset();
    }
  }, true);
  window.addEventListener('storage', e => {
    if (e.key !== key) return;
    try { const saved = JSON.parse(e.newValue); if (valid(saved)) { state = saved; render(); } } catch (_) {}
  });
  const observer = new MutationObserver(() => { render(); positionOverlay(); });
  slides.forEach(s => observer.observe(s, { attributes: true, attributeFilter: ['class'] }));
  const resize = new ResizeObserver(positionOverlay);
  slides.forEach(s => resize.observe(s));
  window.addEventListener('resize', positionOverlay);
  document.addEventListener('fullscreenchange', positionOverlay);
  setInterval(render, 100);
  render();
  positionOverlay();
}

// Functional engines receive Marp CLI's own engine, so no additional npm
// dependency (or a separately installed @marp-team/marp-core) is required.
module.exports = ({ marp }) => {
  const minutes = Number(process.env.RABBIT_MINUTES ?? 5);
  if (!Number.isFinite(minutes) || minutes <= 0 || minutes >= 1440 ||
      Math.round(minutes * 60000) < 1000) {
    throw new Error('RABBIT_MINUTES must specify at least one second and less than 1440 minutes.');
  }
  const duration = Math.round(minutes * 60000);
  const literal = value => JSON.stringify(value).replace(/</g, '\\u003c');
  // Only SCRIPT is inserted into the rendered deck: Bespoke excludes SCRIPT
  // children from its slide list. Create UI outside the deck after Marp has
  // initialized; inserting an ASIDE directly would create an extra slide.
  const bootstrap = `<script data-rabbit-engine>
(() => {
  const initialize = () => (${initializeTimer.toString()})(${duration}, ${literal(TIMER_STYLE)}, ${literal(TIMER_MARKUP)});
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initialize, { once: true });
  } else initialize();
})();
</script>`;
  const render = marp.render.bind(marp);
  marp.render = (...args) => {
    const result = render(...args);
    // Printable conversion requests individual slide HTML, rather than a
    // Bespoke deck. Keep the interactive timer out of those render results.
    if (typeof result.html === 'string') result.html += bootstrap;
    return result;
  };
  return marp;
};
