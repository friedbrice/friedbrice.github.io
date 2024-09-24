class Block < Liquid::Block
  def initialize(tag_name, input, tokens)
    super
    @input = input
  end

  def render(context)
    site = context.registers[:site]
    converter = site.find_converter_instance(::Jekyll::Converters::Markdown)
    content = converter.convert(super)
    inputs = @input.split(' ', 2)
    class_name = inputs[0]
    title = converter.convert(inputs[1])
    caption = ""

    if title then
      caption = "<figcaption>#{title}</figcaption>"
    end

    <<-HTML.gsub /^\s+/, '' # remove whitespaces from heredocs
    <figure class="#{class_name}">
      #{caption}
      #{content}
    </figure>
    HTML
  end
end

Liquid::Template.register_tag('block', Block)
