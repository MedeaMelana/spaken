package spaken.model;

import spaken.model.rendered.Rendered;

public class Group implements Element {
	private PluggablePoint[] inputPoints;

	private Element outputElement;
	
	private Group(Element outputElement) {
		//TODO rebuild element with PluggablePoints as the bottom dependencies
	}

	public Element[] getDependencies() {
		// TODO kopie maken
		return inputPoints;
	}

	public PluggablePoint[] getInputPoints() {
		// TODO kopie maken
		return inputPoints;
	}

	public Element getOutputElement() {
		// TODO multiple elements (but doesn't currently fit with render()
		// returning a single Rendered).
		return outputElement;
	}

	public Rendered render() throws ImaginaryPointException {
		return outputElement.render();
	}
}
