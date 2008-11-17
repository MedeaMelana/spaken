package spaken.model.elements;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import spaken.model.Element;
import spaken.model.ImaginaryPointException;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedGroup;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;
import spaken.util.ListWithoutDuplicates;

// TODO differentiate between a Theorem and an applied Theorem
public class Theorem implements Element<Theorem> {
	private List<AssumedPoint> assumptions;
	private List<Element<?>> targetElements;
	
	public Theorem(Element<?> target) {
		List<Element<?>> targets = new LinkedList<Element<?>>();
		targets.add(target);
		init(targets);
	}
	
	public Theorem(List<Element<?>> targets) {
		init(targets);
	}
	
	private void init(List<Element<?>> targets) {
		targetElements = new ArrayList<Element<?>>(targets.size());
		assumptions = new ListWithoutDuplicates<AssumedPoint>();
		for (Element<?> e : targets) {
			// TODO problem with shared AssumedPoints and copying!
			e = e.copyElement();
			targetElements.add(e);
			e.collectAssumptions(assumptions);
		}
		assumptions = new ArrayList<AssumedPoint>(assumptions);
	}
	
	public List<AssumedPoint> getAssumptions() {
		return Collections.unmodifiableList(assumptions);
	}

	public void collectAssumptions(Collection<AssumedPoint> list) {
		list.addAll(assumptions);
	}

	public Theorem copyElement() {
		return new Theorem(targetElements);
	}

	public void readElement(ElementReader in) throws IOException {
		targetElements = (List<Element<?>>) in.readRefs();
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRefs(targetElements);
	}
	
	public Rendered render() throws ImaginaryPointException {
		return RenderedGroup.renderAll(targetElements);
	}

}
