<!-- What goes in this file will appear on near the end of <head>-->
<link href="/static/style.css?version=202404191200110200" rel="stylesheet" />
<link href="/static/catppuccin-latte.css?version=202404191200110200" rel="stylesheet" />

<script src="https://cdn.jsdelivr.net/npm/prismjs/prism.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/prismjs/components/prism-nix.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/prismjs/plugins/filter-highlight-all/prism-filter-highlight-all.min.js"></script>
<script>
Prism.plugins.filterHighlightAll.reject.add(value => value.language === "mermaid");
</script>
<script src="https://cdn.jsdelivr.net/npm/prismjs/plugins/autoloader/prism-autoloader.min.js"></script>

<script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
<script>
window.addEventListener("load", (event) => {
  const mermaids = document.querySelectorAll('code.language-mermaid');
  console.log('hello mermaids', mermaids)
  mermaid.init(undefined, mermaids);
  for (const code of mermaids) {
    console.log(code)
    code.style.background = 'initial';
    const pre = code.parentNode;
    pre.style.border = 'none';
    pre.style.textAlign = 'center';
    pre.style.background = 'initial';
  }
});
</script>

<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex/dist/katex.min.css" />

<!-- The loading of KaTeX is deferred to speed up page rendering -->
<script defer src="https://cdn.jsdelivr.net/npm/katex/dist/katex.min.js"></script>

<!-- To automatically render math in text elements, include the auto-render extension: -->
<script defer src="https://cdn.jsdelivr.net/npm/katex/dist/contrib/auto-render.min.js"
    onload="renderMathInElement(document.body);"></script>

<!-- Prefetch KaTeX fonts to prevent FOUT or FOIT. -->
<script defer src="https://cdn.jsdelivr.net/npm/webfontloader/webfontloader.js"></script>
