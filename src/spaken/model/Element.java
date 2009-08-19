/* Created on Jun 20, 2008. */
package spaken.model;

import java.awt.Graphics2D;
import java.util.Collection;

import spaken.model.elements.AssumedPoint;
import spaken.util.Collector;

public interface Element<T extends Element> {
	/*
	 * TODO Elements cannot currently safely implement an equals method, because
	 * FlatFileStorage assumes, by using a HashMap, that equals does the same as
	 * ==. Fix this first if you want equals.
	 */

	//TODO deze maken:
	public void draw(Graphics2D g, double pixelSize);

	public void collectAssumptions(Collector<AssumedPoint> collect);
	
	public void addElementListener(ElementListener<? super T> d);
	public void removeElementListener(ElementListener<? super T> d);
	
	/**
	 * Determines on which other <tt>Element</tt>s this <tt>Element</tt> depends.
	 * @param collect A collection into which to collect the dependencies.
	 */
	public void collectDependencies(Collection<Element<?>> collect);
	
	public boolean isExported();
	public void setExported(boolean export);
	
	public Theorem getTheorem();
	// niet zomaar aanroepen
	public void theoremChanged(Theorem theorem);
	
	public T duplicate();
	
//	public T instantiate(PointBinding<Point> binding)
//			throws UnboundPointException;

	/**
	 * Write this <tt>Element</tt> using the specified <tt>ElementWriter</tt>.
	 * Call this method whenever you like.
	 * 
	 * @param out
	 *            The <tt>ElementWriter</tt> that should be filled with data.
	 * @throws IOException
	 */
//	public void writeElement(ElementWriter out) throws IOException;

	/**
	 * Restore this <tt>Element</tt> using the specified
	 * <tt>ElementReader</tt>. This method is only to be called by
	 * <tt>StorageEngine</tt>s, and only on newly created instances. It is
	 * extremely inpolite to call this method from elsewhere.
	 * 
	 * TODO Think about alternative: require a constructor that takes an
	 * ElementReader. This is more reflection-heavy and cannot be required by
	 * the interface.
	 * 
	 * @param in
	 *            The <tt>ElementReader</tt> that contains the data for
	 *            restoration.
	 * @throws IOException
	 */
//	public void readElement(ElementReader in) throws IOException;

}
