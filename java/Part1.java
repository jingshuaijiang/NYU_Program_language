import java.util.*;
import java.lang.*;

//definition of class ComparableList<T>
class ComparableList<T extends Comparable<T>> extends ArrayList<T> implements Comparable<ComparableList<T>>{
	public int compareTo(ComparableList<T> l2)
	{
		Iterator<T> IT1 = this.iterator();
		Iterator<T> IT2 = l2.iterator();
		while(IT1.hasNext()&&IT2.hasNext())
		{
			T t1 = IT1.next();
			T t2 = IT2.next();
			int result = t1.compareTo(t2);
			if(result==0)
			{
				continue;
			}
			else{
				return result;
			}
		}
		if(this.size()>l2.size())
		{
			return 1;
		}
		else if(this.size()<l2.size())
		{
			return -1;
		}
		else return 0;
	}
	public String toString()
	{
		String s = "[[";
		for(T t:this)
		{
			s=s+t+" ";
		}
		s=s+"]]";
		return s;
	}
}

//definition of class A
class A implements Comparable<A>{
	Integer value;
	public A(Integer x)
	{
		this.value = x;
	}

	public int compareTo(A a2)
	{
		if(this.value > a2.value)
		{
			return 1;
		}
		else if(this.value < a2.value)
		{
			return -1;
		}
		else
		{
			return 0;
		}
	}
	public String toString()
	{
		return "A<"+value.toString()+">";
	}
}

//definition of class B
class B extends A{
	Integer xvalue;
	Integer yvalue;

	public B(Integer x, Integer y)
	{
		super(x+y);
		this.xvalue = x;
		this.yvalue = y;
	}

	public int compareTo(A a2)
	{
		if(this.value > a2.value)
		{
			return 1;
		}
		else if(this.value < a2.value)
		{
			return -1;
		}
		else
		{
			return 0;
		}
	}
	public String toString()
	{
		return "B<"+xvalue.toString()+","+yvalue.toString()+">";
	}
}

public class Part1
{

	public static <T> void addToCList(T z,ComparableList<? super T> L)
	{
		L.add(z);
	}


	public static void main(String args[])
	{
		test();
	}
	static void test() {
	ComparableList<A> c1 = new ComparableList<A>();
	ComparableList<A> c2 = new ComparableList<A>();
	for(int i = 0; i < 10; i++) {
	    addToCList(new A(i), c1);
	    addToCList(new A(i), c2);
	}
	
	addToCList(new A(12), c1);
	addToCList(new B(6,6), c2);
	
	addToCList(new B(7,11), c1);
	addToCList(new A(13), c2);

	System.out.print("c1: ");
	System.out.println(c1);
	
	System.out.print("c2: ");
	System.out.println(c2);

	switch (c1.compareTo(c2)) {
	case -1: 
	    System.out.println("c1 < c2");
	    break;
	case 0:
	    System.out.println("c1 = c2");
	    break;
	case 1:
	    System.out.println("c1 > c2");
	    break;
	default:
	    System.out.println("Uh Oh");
	    break;
	}

    }
}