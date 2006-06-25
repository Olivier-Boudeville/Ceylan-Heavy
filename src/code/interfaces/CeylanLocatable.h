#ifndef CEYLAN_LOCATABLE_H_
#define CEYLAN_LOCATABLE_H_


#include "CeylanEventSource.h"      // for EventSource
#include "CeylanEventListener.h"    // for EventListener
#include "CeylanTextDisplayable.h"  // for TextDisplayable


#include <string>


namespace Ceylan
{


	namespace Maths
	{
	
	
		namespace Linear
		{
	
			// Forward declaration.
			class Matrix ;
	
		}
	
	}
	
	
	/// Exception to raise when Locatables are in trouble.
	class LocatableException : public Ceylan::Exception
	{
	
		public:
		
			LocatableException( const std::string & message ) throw() ;
			virtual ~LocatableException() throw() ;
	
	} ;



	/**
	 * Events used by a Locatable to notify its Locatable children 
	 * that its referential changed.
	 *
	 */
	class ReferentialChangedEvent : public Event
	{
	
		public:
		
			explicit ReferentialChangedEvent( EventSource & source ) throw() ;
			virtual ~ReferentialChangedEvent() throw() ;
	
	} ;



	/**
	 * Objects that should be locatable in space should inherit from
	 * this class.
	 *
	 * Being Locatable is the synonym of having a referential, either local,
	 * i.e. defined relatively to another father referential, or absolute,
	 * i.e. directly expressed in 'world' coordinate.
	 *
	 * Each Locatable owns two referentials, one, as explained before,
	 * relative to its father (called the local referential), and one that
	 * precomputes all the transformations between this referential and the
	 * root of the referential tree, i.e. the world referential. This 
	 * second referential is called the global referential for the given 
	 * Locatable instance.
	 *
	 * A Locatable embeds two matrices, one for each of the two referentials.
	 * These matrices should be in most cases homogeneous matrices, allowing
	 * the Locatable to define a referential that locates this object 
	 * relatively to its parent referential, both in terms of position and 
	 * orientation.
	 * 
	 * The dimension of the matrix depends on the space into which the 
	 * Locatable is defined (ex : 2D, 3D), and on whether an homogeneous
	 * matrix is used. In this case, the matrix dimension must be
	 * incremented : for instance, Locatable instances in a 2D world should 
	 * embed two 3x3 homogeneous matrices.
	 *
	 * A referential is defined by both a point in space and a set of 
	 * angles or vectors that indicates its orientation : from that 
	 * information, the relevant transformation (combination of translations
	 * and rotations) can be computed.
	 *
	 * The local referential is a matrix Mlf that transforms points expressed
	 * in the local referential Pl into points expressed in the father 
	 * referential Pf : Pf = Mlf.Pl  
	 *
	 * The referentials are organized as a tree, where the children of a
	 * node have referentials that are defined relatively to this node's
	 * referential. 
	 *
	 * There may be only one tree, therefore one root, so that taking into
	 * account the camera is easier : the global matrix, root of the 
	 * referential hierarchy, may have for matrix the camera's one, so that the
	 * computed global matrices have all the necessary transformations
	 * registered, in order to perform all steps at once, including camera
	 * management. 
	 *
	 * The up-to-date state of a Locatable's global referential 
	 * (world to local) is managed.
	 * Starting from a situation where all global referentials are 
	 * up-to-date, a change of a node (its local referential changes 
	 * relatively to its father) into the Locatable hierarchy triggers 
	 * the following : its up-to-date state is set to false, therefore
	 * preventing the use of its now outdated global referential, and 
	 * all the subtree it is the root of is recursively set as 
	 * 'not up-to-date'. A property flows from it : there is no need to
	 * propagate 'not up-to-date' status when encountering a node which 
	 * was already not up-to-date, since all its children must already be 
	 * in 'not up-to-date' state. 
	 *
	 * @see The <code>changed</code> method.
	 *
	 * For a node having registered the fact that its global referential
	 * is not up-to-date, but needing this global referential, it has to 
	 * ask its father for its own global referential.
	 * The father node, depending on its up-to-date state, recomputes or
	 * returns directly its global referential. 
	 * Then this node, from this returned father global referential and using
	 * its own new local referential, computes its new global referential. 
	 * It is, from now on, up-to-date until a change occurs, at its level or
	 * in the path from itself to its root referential.
	 *
	 * @note Some methods, such as 
	 * <code>setCenter(const Point & newCenter)</code> could be proposed  
	 * here, but it would involve dynamic casting, which may fail 
	 * (ex : <code>myPoint2D.setCenter( aPoint3D )</code>), 
	 * therefore they all would have to throw exceptions, which should would
	 * have to be caught, etc. 
	 * To avoid such complications, these operators are defined specifically
	 * for each child class, instead of being defined generally in this 
	 * root abstract class : a simpler approach is to define them 
	 * appropriately, at the right level, the implementation one.
	 *
	 * @note Always remember to call <code>setUpToDateState(false)</code>
	 * when any change is done on the local referential.
	 *
	 */
	class Locatable : public EventSource, public EventListener
	{
	

		public:

			
			/**
			 * Constructs a new Locatable, defined relatively to its specified  
			 * <b>fatherLocatable</b>.
			 *
			 * @note The father Locatable is not owned by this object,
			 * which has only a reference onto it. Reciprocally, this 
			 * Locatable will register itself to its father, so that
			 * it can it can be notified of its father's change.
			 *
			 * @note This is not meant to be a copy constructor. Father
			 * locatable cannot be 'const' since registering to it is a
			 * non-const operation.
			 *
			 */
			explicit Locatable( Locatable & fatherLocatable ) throw() ;
			
			
			/**
			 * Basic constructor, no father referential registered, 
			 * therefore considered as an absolute one.
			 *
			 */
			Locatable() throw() ;


			/**
			 * Constructs a new Locatable, defined relatively to its 
			 * specified father Locatable, starting with specified local
			 * referential.
			 *
			 * @param fatherLocatable the referential this Locatable will
			 * be defined relatively to.
			 *
			 * @param localReferential the transformation matrix that
			 * converts vectors expressed in the referential defined by
			 * this Locatable to vectors expressed in the referential 
			 * defined by the parent Locatable. This Locatable takes 
			 * ownership of the specified matrix. 
			 *
			 * @note The father Locatable is not owned by this object,
			 * which has only a reference onto it. Reciprocally, this 
			 * Locatable will register itself to its father, so that
			 * it can it can be notified of its father's change.
			 *
			 * @note Father locatable cannot be 'const' since registering 
			 * to it is a non-const operation.
			 *
			 * @note The Locatable will take ownership of the specified
			 * referential, hence will deallocate it when appropriate.
			 *
			 */
			explicit Locatable( Locatable & fatherLocatable, 
				Maths::Linear::Matrix & localReferential ) throw() ;
			
			
			/**
			 * Basic constructor, no father referential registered, this
			 * referential is therefore considered as an absolute one. 
			 *
			 * Its local referential, which is in this particular case 
			 * a global one too, is specified. 
			 *
			 * @note The Locatable will take ownership of the specified
			 * referential.
			 *			 
			 */
			explicit Locatable( Maths::Linear::Matrix & localReferential )
				throw() ;

			
			/// Virtual destructor.
			virtual ~Locatable() throw() ;
			
			
			
			/**
			 * Returns whether this Locatable is defined absolutely
			 * (returns true) or relatively to a father referential
			 * (returns false).
			 *
			 * @note Only the root referential is absolute. For the world
			 * tree, the local referential of absolute referentials 
			 * corresponds to the camera matrix.
			 *
			 */
			virtual bool isAbsolute() const throw() ;
			
			
			/**
			 * Tells whether this Locatable has a local referential available.
			 *
			 * @note It should be the case in most situations.
			 *
			 */
			virtual bool hasLocalReferential() const throw() ;
			
			
			/**
			 * Returns this Locatable's referential, expressed in father's
			 * space, i.e. returns the local referential of this Locatable
			 * 'as is'.
			 *
			 * @throw LocatableException if no local referential is available.
			 *
			 */
			virtual Maths::Linear::Matrix & getLocalReferential() const 
				throw( LocatableException ) ;
		
		
			/**
			 * Sets the local referential thanks to specified matrix.
			 *
			 * @note This Locatable takes ownership of the specified matrix.
			 *
			 */
			virtual void setLocalReferential( 
				Maths::Linear::Matrix & newGlobalReferential ) throw() ; 
				
			
			/**
			 * Blanks local referential, so that its matrix is the null matrix.
			 *
			 * @note If no referential was existing, a new one is created.
			 *
			 * This method is pure virtual since blanking and creation 
			 * depend on the inner matrix.
			 *
			 */	
			virtual void blankLocalReferential() throw() = 0 ;
			
				
			/**
			 * Tells whether this Locatable has a valid global referential
			 * available.
			 *
			 */
			virtual bool hasGlobalReferential() const throw() ;
			
		
			/**
			 * Returns this Locatable's referential, expressed in global
			 * (world) space.
			 *
			 * @return the internal up-to-date global referential. It is still
			 * owned by the Locatable.
			 *
			 * @note Everything necessary is done to return the global
			 * referential, be it already available, up-to-date, etc., or 
			 * not.
			 *
			 */
			virtual Maths::Linear::Matrix & getGlobalReferential()
				throw( LocatableException ) ;
		
		
			/**
			 * Tells whether this Locatable's referential is deemed 
			 * up-to-date in world (global) referential, i.e. if all the 
			 * Locatable instances from this one to the root of its tree
			 * are up-to-date, bounds included.
			 *
			 */
			virtual bool isUpToDate() const throw() ;
			
			
			/**
			 * Assigns the up-to-date state of this Locatable, mostly used
			 * by father referentials to propagate a change down the 
			 * referential tree.
			 *
			 */
			virtual void setUpToDateState( bool newState ) throw() ;

			  
			/**
			 * Notifies this Locatable of a new event.
			 *
			 * @note Only ReferentialChangedEvents are taken into account,
			 * others are ignored.
			 *
			 * @note This event remains property of the EventSource, 
			 * which will take care of its life cycle.
			 *
			 */
			virtual void beNotifiedOf( const Event & newEvent ) throw() ;

			 
            /**
             * Returns a user-friendly description of the state of this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
             virtual const std::string toString( 
			 	VerbosityLevels level = high ) const throw() ;
				
				
				
		protected:
		
		
			/**
			 * Method to be called when inner referential changed. 
			 * 
			 * Its role is to propagate a corresponding 
			 * ReferentialChangedEvent to this Locatable's children.
			 *
			 */
			 virtual void changed() throw() ;
			 
			
			/**
			 * Updates this Locatable's state (actually its global 
			 * referential) from specified matrix, which has to be the
			 * up-to-date version of this Locatable's father global 
			 * referential.
			 *
			 * @note Basically, it consists on applying the father
			 * transformation to the local one to have a fully world-to-local
			 * precomputed transformation. 
			 *
			 * @note This is a pure virtual method since only the relevant 
			 * Matrix type shall be called (ex : Matrix3).
			 *
			 */ 
			virtual void updateFromFather( 
				const Maths::Linear::Matrix & upToDateFatherReferential )
					throw() = 0 ;
			
			
			/**
			 * Detaches this Locatable from its father :  their referentials
			 * will not be synchronized anymore.
			 *
			 * @throw LocatableException if this Locatable had no father 
			 * or was not linked to it.
			 *
			 */
			virtual void detachFromFather() throw( LocatableException ) ;
			
			
			/**
			 * Returns a generic description of the Locatable.
			 *
			 * Useful to share this part of the description among the children
			 * to ease toString management.
			 *
			 */
			virtual const std::string describe( VerbosityLevels level ) 
				const throw() ;
				
			 
			/**
			 * This referential's father, if any.
			 *
			 * @note The father is not owned by any of its children.
			 *
			 */
			Locatable * _father ;
			
			 
			/**
			 * The internal referential corresponding to this Locatable,
			 * expressed in father's space.
			 *
			 * @note Even the local referential must be a pointer, since 
			 * Matrix is abstract.
			 *
			 */			 
			Maths::Linear::Matrix * _localReferential ;
			
			
			/**
			 * Pre-computed referential, from world space (global) to this
			 * referential.
			 *
			 * @note This referential may be blank, as long as its 
			 * computation is not requested.
			 *
			 */
			Maths::Linear::Matrix * _globalReferential ;
			
			
			
			
			/**
			 * This internal event is allocated the first time this Locatable
			 * changes its referential, one time for all : for next changes,
			 * that same event will be changed accordingly abd then 
			 * propagated again.
			 *
			 */
			ReferentialChangedEvent * _changedEvent ;



		private:

		
			/**
			 * Tells whether the internal referential is up-to-date.
			 *
			 * @note Not private since child classes will have to manage
			 * it specifically as well.
			 *
			 */	
			bool _isUpToDate ;

		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Locatable( const Locatable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Locatable & operator = ( const Locatable & source ) throw() ;
		
				
	} ;
	
}	
	



#endif // CEYLAN_LOCATABLE_H_
