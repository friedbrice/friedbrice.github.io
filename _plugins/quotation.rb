class Quotation < Liquid::Block
  def initialize(tag_name, input, tokens)
    super
    @input = input
  end

  def render(context)
    site = context.registers[:site]
    converter = site.find_converter_instance(::Jekyll::Converters::Markdown)
    content = super

    <<-HTML.gsub /^\s+/, '' # remove whitespaces from heredocs
    <figure class="quotation">
      <blockquote>
        #{converter.convert(content)}
      </blockquote>
      <figcaption>#{@input}</figcaption>
    </figure>
    HTML
  end
end

Liquid::Template.register_tag('quotation', Quotation)
