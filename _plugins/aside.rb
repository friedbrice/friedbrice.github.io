class Aside < Liquid::Block

  def render(context)
    site = context.registers[:site]
    converter = site.find_converter_instance(::Jekyll::Converters::Markdown)
    content = super

    <<-HTML.gsub /^\s+/, '' # remove whitespaces from heredocs
    <aside class="note">
      #{converter.convert(content)}
    </aside>
    HTML
  end
end

Liquid::Template.register_tag('aside', Aside)
