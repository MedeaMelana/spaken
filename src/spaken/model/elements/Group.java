package spaken.model.elements;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import spaken.model.*;
import spaken.model.rendered.Rendered;
import spaken.storage.ElementReader;
import spaken.storage.ElementWriter;

public class Group implements Element<Group> {
	private List<PluggablePoint> inputPoints;

	private Element<?> outputElement;
	
	public Group(Element<?> outputElement) {
		//TODO see if this has to become non-destructive
		List<PluggablePoint> points = new LinkedList<PluggablePoint>();
		this.outputElement = outputElement.makePluggableCopy(points);
		
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

	public Group makePluggableCopy(List<PluggablePoint> collect) {
		// TODO Think about this. This is just a guess.
		//outputElement.makePluggableCopy(collect);
		
		return new Group(outputElement);
	}
	
	public void writeElement(ElementWriter out) throws IOException {
		out.writeRefs(inputPoints);
		out.writeRef(outputElement);
	}
	
	public void readElement(ElementReader in) throws IOException {
		for (Element e : in.readRefs()) {
			inputPoints.add((PluggablePoint) e);
		}
	}
}
