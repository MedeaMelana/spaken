package spaken.model;

import java.util.*;

import spaken.model.elements.*;

public class Theorem {
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

	public Group applyTheorem(final List<Point> points)
			throws UnboundPointException {
		return applyTheorem(new ListPointBinding<Point>(points));
	}

	public Group applyTheorem(final PointBinding<Point> binding)
			throws UnboundPointException {
		PointBinding<Point> newBinding = new PointBinding<Point>() {

			public Point getBindingFor(int index) throws UnboundPointException {
				for (int i = 0; i < assumptions.size(); i++) {
					if (assumptions.get(i).getIndex() == index) {
						return binding.getBindingFor(i);
					}
				}

				throw new UnboundPointException(index);
			}

			public void setBindingFor(int index, Point pos)
					throws UnboundPointException {
				// TODO
				throw new UnsupportedOperationException();
			}

			public int createBinding(Point p) {
				// TODO
				throw new UnsupportedOperationException();
			}

		};

		/*
		 * This copies all targets individualy, which results in duplicating all
		 * shared Elements. I think this is safe, but it doesn't feel desirable.
		 */
		List<Element<?>> newTargets = new ArrayList<Element<?>>(targetElements
				.size());
		for (Element<?> e : targetElements) {
			newTargets.add(e.instantiate(newBinding));
		}

		return new Group(newTargets);
	}

}
