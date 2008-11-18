package spaken.model.elements;

import java.io.IOException;
import java.util.*;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedGroup;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class AppliedTheorem implements Element<AppliedTheorem> {

	private Theorem sourceTheorem;

	private List<Element<?>> targetElements;

	AppliedTheorem(Theorem theorem, List<Element<?>> targets) {
		this.sourceTheorem = theorem;
		this.targetElements = targets;
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		for (Element<?> e : targetElements) {
			e.collectAssumptions(collect);
		}
	}

	public AppliedTheorem instantiate(PointBinding binding)
			throws UnboundPointException {
		List<Element<?>> newTargets = new ArrayList<Element<?>>(targetElements
				.size());

		// same un-aliasing happens as in Theorem
		for (Element e : targetElements) {
			newTargets.add(e.instantiate(binding));
		}

		// does not instantie the theorem
		return new AppliedTheorem(sourceTheorem, newTargets);
	}

	public Rendered render(PointBinding binding)
			throws ImaginaryPointException, UnboundPointException {
		return RenderedGroup.renderAll(targetElements, binding);
	}

	public void readElement(ElementReader in) throws IOException {
		sourceTheorem = (Theorem) in.readRef();
		targetElements = in.readRefs();
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRef(sourceTheorem);
		out.writeRefs(targetElements);
	}

}
