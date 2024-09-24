require 'nokogiri'

class Figure < Liquid::Block
  def initialize(tag_name, input, tokens)
    super
    @input = input
  end

  def render(context)
    site = context.registers[:site]
    converter = site.find_converter_instance(::Jekyll::Converters::Markdown)
    content = super
    html_content = converter.convert(content)
    text_content = Nokogiri::HTML5.fragment(html_content).content

    <<-HTML.gsub /^\s+/, '' # remove whitespaces from heredocs
    <figure>
      <img
        src="#{@input}"
        alt="#{text_content}"
      />
      <figcaption>
        #{html_content}
      </figcaption>
    </figure>
    HTML
  end
end

Liquid::Template.register_tag('figure', Figure)
