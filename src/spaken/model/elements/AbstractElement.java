package spaken.model.elements;

import java.util.*;

import spaken.model.Element;
import spaken.model.Dependency;

public abstract class AbstractElement<E extends Element> implements Element<E> {

	private List<Dependency<? super E>> dependencies;
	
	public AbstractElement() {
		dependencies = new LinkedList<Dependency<? super E>>();
	}
	
	public void addDependency(Dependency<? super E> e) {
		dependencies.add(e);
	}

	public void removeDependency(Dependency<? super E> e) {
		dependencies.add(e);
	}
	
	// TODO elem hoort eigenlijk altijd this te zijn, maar daar willen de generics niet aan.
	protected void notifyDependencies(E elem) {
		for (Dependency<? super E> dependency : dependencies) {
			dependency.elementChanged(elem);
		}
	}
	
	protected Collection<Dependency<? super E>> getDependencies() {
		return Collections.unmodifiableCollection(dependencies);
	}

}
