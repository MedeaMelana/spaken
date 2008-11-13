package spaken.model;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import spaken.model.rendered.Rendered;

public class Group implements Element {
	private List<PluggablePoint> inputPoints;

	private Element outputElement;
	
	public Group(Element outputElement) {
		//TODO see if this has to become non-destructive
		List<PluggablePoint> points = new LinkedList<PluggablePoint>();
		outputElement.makePluggable(points);
		
		this.outputElement = outputElement;
		
		// make a copy (in a more efficient form)
		this.inputPoints = new ArrayList<PluggablePoint>(points);
	}

	public Element[] getDependencies() {
		return getInputPoints();
	}

	public PluggablePoint[] getInputPoints() {
		return inputPoints.toArray(new PluggablePoint[inputPoints.size()]);
	}

	public Element getOutputElement() {
		// TODO multiple elements (but doesn't currently fit with render()
		// returning a single Rendered).
		return outputElement;
	}

	public Rendered render() throws ImaginaryPointException {
		return outputElement.render();
	}

	public void makePluggable(List<PluggablePoint> collect) {
		// TODO Think about this. This is just a guess.
		collect.addAll(inputPoints);
	}
}
