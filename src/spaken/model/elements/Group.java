package spaken.model.elements;

import java.io.IOException;
import java.util.*;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedGroup;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Group implements Element<Group> {

	private List<Element<?>> elements;

	public Group() {

	}

	public Group(Collection<Element<?>> elements) {
		this.elements = new LinkedList<Element<?>>(elements);
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		for (Element<?> e : elements) {
			e.collectAssumptions(collect);
		}
	}

	public Group instantiate(PointBinding<Point> binding)
			throws UnboundPointException {
		List<Element<?>> newElements = new LinkedList<Element<?>>();

		for (Element<?> e : elements) {
			newElements.add(e.instantiate(binding));
		}

		Group group = new Group();
		group.elements = newElements;

		return group;
	}

	public Rendered render(PointBinding<Pos> binding)
			throws ImaginaryPointException, UnboundPointException {
		return RenderedGroup.renderAll(elements, binding);
	}

	public void readElement(ElementReader in) throws IOException {
		elements = in.readRefs();
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRefs(elements);
	}

	public List<Element<?>> getElements() {
		return Collections.unmodifiableList(elements);
	}

}
