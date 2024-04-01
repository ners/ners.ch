<ema:note:pandoc>
  <Para>
    <p>
      <inlines />
    </p>
  </Para>
  <Task:Checked>
    <!-- FIXME: Fix list styling to use flexbox, so task lists don't botch them up -->
    <apply template="/templates/components/checkbox-checked">
      <inlines />
    </apply>
  </Task:Checked>
  <Task:Unchecked>
    <apply template="/templates/components/checkbox-unchecked">
      <inlines />
    </apply>
  </Task:Unchecked>
  <Cite>
    <cite>
      <inlines />
    </cite>
  </Cite>
  <BlockQuote>
    <blockquote>
      <blocks />
    </blockquote>
  </BlockQuote>
  <DefinitionList>
    <dl class="flex flex-col mb-3">
      <DefinitionList:Items>
        <div class="my-1">
          <dt class="font-bold text-l">
            <DefinitionList:Item:Term />
          </dt>
          <DefinitionList:Item:DescList>
            <div class="flex flex-col pl-1">
              <dd class="pl-2 my-1 text-gray-700 border-l-2">
                <DefinitionList:Item:Desc />
              </dd>
            </div>
          </DefinitionList:Item:DescList>
        </div>
      </DefinitionList:Items>
    </dl>
  </DefinitionList>
  <Note:Ref>
    <sup class="px-0.5">
      <a class="text-${theme}-600 hover:underline" href="${ema:note:url}#fn${footnote:idx}">
        <footnote:idx />
      </a>
    </sup>
  </Note:Ref>
  <Note:List>
    <div title="Footnotes" class="footnotes">
      <header class="font-semibold">Footnotes</header>
      <ul>
        <footnote>
          <li id="fn${footnote:idx}">
            <p><i><footnote:idx /></i></p>
            <div><p><footnote:content /></p></div>
          </li>
        </footnote>
      </ul>
    </div>
  </Note:List>

  <BulletList>
    <ul class="my-3 ml-6 space-y-1 list-disc">
      <BulletList:Items>
        <li>
          <BulletList:Item />
        </li>
      </BulletList:Items>
    </ul>
  </BulletList>
  <OrderedList>
    <ul class="my-3 ml-6 space-y-1 list-decimal list-inside">
      <OrderedList:Items>
        <li>
          <OrderedList:Item />
        </li>
      </OrderedList:Items>
    </ul>
  </OrderedList>
  <HorizontalRule>
    <hr class="mb-3" />
  </HorizontalRule>
  <!-- TODO: Expand the above kind of overriding (full DOM control) to other AST nodes (below) -->
  <PandocLink class="text-${theme}-600">
    <Internal class="mavenLinkBold hover:underline" />
    <External class="hover:underline" target="_blank" rel="noopener" />
  </PandocLink>
  <CodeBlock />
  <Code class="py-0.5 px-0.5 bg-gray-100" />
  <Header>
    <h1 class="pb-2 mb-2 font-bold text-center" />
    <h2 class="mt-8 mb-6 font-bold text-700" />
    <h3 class="mt-6 mb-2 font-bold text-700" />
    <h4 class="mt-6 mb-2 font-bold text-700" />
    <h5 class="mt-6 mb-2 font-bold text-700" />
    <h6 class="mt-6 mb-2 font-bold text-700" />
  </Header>

</ema:note:pandoc>
