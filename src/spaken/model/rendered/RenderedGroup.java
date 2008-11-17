package spaken.model.rendered;

import java.awt.Graphics2D;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import spaken.model.Element;
import spaken.model.ImaginaryPointException;

public class RenderedGroup implements Rendered {
	private List<Rendered> rendereds;
	
	public RenderedGroup(Collection<Rendered> rendereds) {
		this.rendereds = new LinkedList<Rendered>(rendereds);
	}
	
	public static RenderedGroup renderAll(Collection<Element<?>> elements) throws ImaginaryPointException {
		LinkedList<Rendered> rendered = new LinkedList<Rendered>();
		
		for (Element e : elements) {
			rendered.add(e.render());
		}
		
		return new RenderedGroup(rendered);
	}
	
	public void draw(Graphics2D g, double pixelSize) {
		for (Rendered r : rendereds) {
			r.draw(g, pixelSize);
		}
	}

}
