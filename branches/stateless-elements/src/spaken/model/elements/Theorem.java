package spaken.model.elements;

import java.io.IOException;
import java.util.*;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.model.rendered.RenderedGroup;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Theorem implements Element<AppliedTheorem> {
	private List<Element<?>> targetElements;

	private List<AssumedPoint> assumptions;

	/**
	 * Only used internally for reading and writing!
	 */
	public Theorem() {
	}

	public Theorem(Element<?> target) {
		List<Element<?>> targets = new ArrayList<Element<?>>(1);
		targets.add(target);
		init(targets);
	}

	public Theorem(List<Element<?>> targets) {
		init(new ArrayList<Element<?>>(targets));
	}

	private void init(List<Element<?>> targets) {
		targetElements = targets;
		Set<AssumedPoint> collect = new HashSet<AssumedPoint>();
		for (Element<?> e : targets) {
			e.collectAssumptions(collect);
		}
		assumptions = new ArrayList<AssumedPoint>(collect);
		Collections.sort(assumptions);
	}

	public int getAssumptionCount() {
		return assumptions.size();
	}

	public AppliedTheorem instantiate(final PointBinding binding)
			throws UnboundPointException {
		PointBinding newBinding = new PointBinding() {

			public Point getPoint(int index) throws UnboundPointException {
				for (int i = 0; i < assumptions.size(); i++) {
					if (assumptions.get(i).getIndex() == index) {
						return binding.getPoint(i);
					}
				}

				throw new UnboundPointException(index);
			}

		};

		/*
		 * This copies all targets individualy, which results in duplicating all
		 * shared Elements. I think this is safe, but it doesn't feel desirable.
		 */
		List<Element<?>> newTargets = new ArrayList<Element<?>>(targetElements
				.size());
		for (Element e : targetElements) {
			newTargets.add(e.instantiate(newBinding));
		}

		return new AppliedTheorem(this, newTargets);
	}

	public void readElement(ElementReader in) throws IOException {
		init(new ArrayList<Element<?>>(in.readRefs()));
	}

	public void writeElement(ElementWriter out) throws IOException {
		out.writeRefs(targetElements);
	}

	public Rendered render(PointBinding binding)
			throws ImaginaryPointException, UnboundPointException {
		return RenderedGroup.renderAll(targetElements, binding);
	}

	public void collectAssumptions(Set<AssumedPoint> collect) {
		collect.addAll(assumptions);
	}
}
